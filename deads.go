package main

import (
	"fmt"
	"encoding/csv"
	"os"
	"log"
	"io"
	"bufio"
	"strings"
	"strconv"
)

func ReadLines(r io.Reader) ([]string, error) {
	var out []string
	s := bufio.NewScanner(r)
	for s.Scan() {
		if (s.Err() != nil) {
			return nil, s.Err()
		}
		out = append(out, s.Text())
	}
	return out, nil
}

func ParseDeads(line []string) (int, error) {
	if len(line) < 1 {
		return 0, fmt.Errorf("line %v too short", line)
	}
	val, err := strconv.Atoi(line[0])
	return val, err
}

func AddDeads(r io.Reader, w io.Writer) error {
	cr := csv.NewReader(r)
	cr.Comma = ' '
	lines, e := cr.ReadAll()
	if e != nil {
		return e
	}
	for _, l := range lines[:len(lines) - 1] {
		if _, e := io.WriteString(w, strings.Join(l, " ")); e != nil {
			return e
		}
	}

	if len(lines) > 0 {
		deads, e := ParseDeads(lines[len(lines) - 1])
		if e != nil {
			return e
		}
		for i := 0; i < deads; i++ {
			if _, e := fmt.Fprintf(w, "0 0 0 0 0 0"); e != nil {
				return e
			}
		}
		// if _, e := io.WriteString(w, strings.Join(lines[len(lines) - 1], " ")); e != nil {
		// 	return e
		// }
	}
	return nil
}

func AddDeadsPath(path string, w io.Writer) error {
	r, err := os.Open(path)
	if err != nil {
		return err
	}
	defer r.Close()
	return AddDeads(r, w)
}

func main() {
	paths, err := ReadLines(os.Stdin)
	if (err != nil) {
		log.Fatal(err)
	}
	for _, path := range paths {
		if err := AddDeadsPath(path, os.Stdout); err != nil {
			log.Fatal(err)
		}
	}
}
