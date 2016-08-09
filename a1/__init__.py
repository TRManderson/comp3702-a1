import argparse


# The argument format provided by the spec is as follows:
# a1-[courseCode]-[ID] environmentMapFileName queryFileName outputFileName
# where courseCode is 3702 and ID is group name (Tomstan)
# the a1-[courseCode]-[ID] is provided to argparse as progname by sys.argv[0]
parser = argparse.ArgumentParser(
    description="COMP3702 Assignment 1",
    epilog="By Tom Manderson and Tristan Roberts",
)


# For information on argparse.FileType, see
# https://docs.python.org/3/library/argparse.html#filetype-objects
parser.add_argument(
    'mapfile',
    type=argparse.FileType(mode='r'),
    help='The environment map filename',
)
parser.add_argument(
    'queryfile',
    type=argparse.FileType(mode='r'),
    help='The query file name',
)
parser.add_argument(
    'outfile',
    type=argparse.FileType(mode='w+'),
    help='The output file name',
)


def main():
    args = parser.parse_args()

if __name__ == "__main__":
    main()
