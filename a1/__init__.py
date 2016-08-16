import argparse
import a1.util as u
from a1.problem import Problem


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
parser.add_argument(
    '--undirected',
    action='store_false',
    default=True,
    dest='directed',
    help='interpret the environment map as an undirected graph'
)
parser.add_argument(
    '--debug',
    action='store_true',
    default=False,
    help='print debug stuff'
)


def main():
    args = parser.parse_args()

    mapdata = args.mapfile.read()
    args.mapfile.close()
    graph = u.graph_from_matrix(
        u.matrix_from_filecontents(
            mapdata
        ),
        directed=args.directed,
    )
    if args.debug:
        print("Environment graph:")
        print(u.stringify_graph(graph))

    querydata = args.queryfile.read()
    args.queryfile.close()
    queries = list(map(
        u.query_from_string,
        map(str.strip, querydata.split("\n")[1:])
    ))

    problem = Problem(graph, debug=args.debug)

    for query in queries:
        args.outfile.write(u.res_to_result(problem.query(query)[1]) + "\n")
    args.outfile.close()


if __name__ == "__main__":
    main()
