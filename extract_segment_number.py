import re
import sys

def extract_with_regex(instream, outstream, wasteregex, pureregex, segmentregex):
	for line in instream:
		stripped_line = line.rstrip("\n")

		wastematch = wasteregex.search(stripped_line)
		if wastematch:
			outstream.write(stripped_line + " " + "waste" + "\n")
			continue

		purematch = pureregex.search(stripped_line)
		if purematch:
			outstream.write(stripped_line + " " + "pure" + "\n")
			continue

		segmentmatch = segmentregex.search(stripped_line)
		if segmentmatch:
			outstream.write(stripped_line + " " + segmentmatch.group(2) + "\n")
			continue

		exit("This is impossible!")

def main():
	wasteregex = re.compile("""waste""")
	pureregex = re.compile("""[Uu]nloaded|pure""")
	segmentregex = re.compile("""(ExtractedS|Segment)([1-6])""")
	extract_with_regex(sys.stdin, sys.stdout, wasteregex, pureregex, segmentregex)

if __name__ == "__main__":
	main()
