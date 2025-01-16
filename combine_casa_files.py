import sys

def read_filenames(instream):
	out = []
	for line in instream:
		stripped_line = line.rstrip("\n")
		out.append(stripped_line)
	return out

def cat_file(filename, outstream):
	out = []
	with open(filename, "r") as r:
		for line in r:
			stripped_line = line.rstrip("\n")
			out.append(stripped_line)
	for line in out[:-1]:
		outstream.write(filename + " " + line + "\n")

def main():
	filenames = read_filenames(sys.stdin)
	for filename in filenames:
		cat_file(filename, sys.stdout)

if __name__ == "__main__":
	main()

