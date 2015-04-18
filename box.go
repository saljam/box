// Package box registers HTTP handlers for a box-joint utility for laser cutting.
package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"strconv"

	_ "github.com/saljam/box/statik"
	"github.com/rakyll/statik/fs"
)

//go:generate statik -src ui

type point struct {
	x, y float64
}

// Actualy this is reflect across x = y.
func rotate(side []point) {
	for i := range side {
		side[i].x, side[i].y = side[i].y, side[i].x
	}
}

func translate(x, y float64, side []point) {
	for i := range side {
		side[i].x += x
		side[i].y += y
	}
}
func reverse(side []point) {
	n := len(side)
	for i := 0; i < (n / 2); i++ {
		side[i], side[n-i-1] = side[n-i-1], side[i]
	}
}

func segmentSizes(length, tabSize float64) (tabWidth, gapWidth float64, n int) {
	segments := int(length / tabSize)
	// We want an odd number of segments
	if segments%2 == 0 {
		segments--
	}

	return length / float64(segments), length / float64(segments), segments
}

// draws a vertical side
func side(length, tabSize, thickness, correction float64, tab, flip bool) []point {
	tabWidth, gapWidth, n := segmentSizes(length, tabSize)

	tabWidth += correction
	firstTab := tabWidth - correction/2
	gapWidth -= correction
	firstGap := gapWidth + correction/2
	// I should probably replace this with a mirror transform...
	if flip {
		tabWidth, gapWidth = gapWidth, tabWidth
		firstTab, firstGap = firstGap, firstTab
	}

	l := make([]point, 0, 0)

	p := point{}
	if tab {
		p.x = 0
		l = append(l, p) // first point
		p.y += firstTab
		l = append(l, p)
	} else {
		p.x = thickness
		l = append(l, p) // first point
		p.y += firstGap
		l = append(l, p)
	}
	tab = !tab

	for i := 1; i < n-1; i++ {
		if tab {
			p.x = 0
			l = append(l, p)
			p.y += tabWidth
			l = append(l, p)
		} else {
			p.x = thickness
			l = append(l, p)
			p.y += gapWidth
			l = append(l, p)
		}
		tab = !tab
	}
	if tab {
		p.x = 0
		l = append(l, p) // first point
		p.y += firstTab
		l = append(l, p)
	} else {
		p.x = thickness
		l = append(l, p) // first point
		p.y += firstGap
		l = append(l, p)
	}
	return l
}

// draws a vertical side
func panel(length, width, tabSize, thickness, correction float64, tabs []bool) []point {
	// left
	left := side(length, tabSize, thickness, correction, tabs[0], false)

	// bottom
	bottom := side(width, tabSize, thickness, correction, !tabs[1], true)
	rotate(bottom)
	translate(0, length-thickness, bottom)

	// right
	right := side(length, tabSize, thickness, correction, !tabs[2], true)
	reverse(right)
	translate(width-thickness, 0, right)

	top := side(width, tabSize, thickness, correction, tabs[3], false)
	rotate(top)
	reverse(top)

	// fixup corners

	top[len(top)-1].x = left[0].x
	left[0].y = top[len(top)-1].y
	left[len(left)-1].y = bottom[0].y
	bottom[0].x = left[len(left)-1].x
	bottom[len(bottom)-1].x = right[0].x
	right[0].y = bottom[len(bottom)-1].y
	right[len(right)-1].y = top[0].y
	top[0].x = right[len(right)-1].x

	panel := append(left, bottom...)
	panel = append(panel, right...)
	panel = append(panel, top...)
	return panel
}

func max(a []int) int {
	m := 0
	for i := range a {
		if a[i] > m {
			m = a[i]
		}
	}
	return m
}

func pathString(path []point) string {
	out := ""
	for _, p := range path {
		out += fmt.Sprintf("%f,%f ", p.x, p.y)
	}
	return out
}

func unzip(p []point) (x, y []int) {
	xs := make([]int, len(p))
	ys := make([]int, len(p))
	for i := range p {
		xs[i] = int(p[i].x)
		ys[i] = int(p[i].y)
	}

	return xs, ys
}

func writeSVG3(w io.Writer, a []point, b []point, c []point) {

	ax, ay := unzip(a)
	maxax := max(ax)
	maxay := max(ay)

	bx, _ := unzip(b)
	maxbx := max(bx)
	_, cy := unzip(c)
	maxcy := max(cy)

	svgw := maxax + maxbx + 20
	svgh := maxay + maxcy + 20

	const style = "stroke-width:0.2mm; fill:none; stroke:black;"
	const polyline = `<g transform="translate(%d,%d)">
<polyline points="%s" style="%s"/>
</g>`
	fmt.Fprintf(w, `<svg width="%[1]dmm" height="%[2]dmm" viewBox="0 0 %[1]d %[2]d" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">`, svgw, svgh)
	fmt.Fprintf(w, polyline, 5, 5, pathString(a), style)
	fmt.Fprintf(w, polyline, 10+maxax, 5, pathString(b), style)
	fmt.Fprintf(w, polyline, 5, 10+maxay, pathString(c), style)
	fmt.Fprintf(w, "</svg>")

}

func handleSVG(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		log.Println(err)
		return
	}
	log.Println("got request for svg", r.Form, "from", r.RemoteAddr)

	length, err := strconv.ParseFloat(r.Form.Get("length"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	width, err := strconv.ParseFloat(r.Form.Get("width"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	height, err := strconv.ParseFloat(r.Form.Get("height"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	tabwidth, err := strconv.ParseFloat(r.Form.Get("tabwidth"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	thickness, err := strconv.ParseFloat(r.Form.Get("thickness"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	correction, err := strconv.ParseFloat(r.Form.Get("correction"), 64)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "image/svg")

	a := panel(length, width, tabwidth, thickness, correction, []bool{true, true, true, true})
	b := panel(length, height, tabwidth, thickness, correction, []bool{false, false, false, false})
	c := panel(height, width, tabwidth, thickness, correction, []bool{true, false, true, false})
	writeSVG3(w, a, b, c)
}

// TODO:
//	- add panel page
//	- write some test cases
func main() {
	http.HandleFunc("/box/svg", handleSVG)
	statikFS, _ := fs.New()
	http.Handle("/", http.FileServer(statikFS))
	http.ListenAndServe(":8080", nil)
}
