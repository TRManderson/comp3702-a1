from typing import Dict, List, Tuple, Iterable
from enum import IntEnum


def matrix_from_filecontents(data: str) -> List[List[float]]:

    return [
        [
            float(elem.strip()) for elem in row.split(" ")
        ]
        for row in data.split("\n")[1:]
    ]


def graph_from_matrix(
        data: List[List[float]],
        undirected: bool = False
) -> Dict[int, Dict[int, float]]:
    g = {}  # type: Dict[int, Dict[int, float]]
    for i, row in enumerate(data, 1):
        for j, weight in enumerate(row, 1):
            if i not in g:
                g[i] = {}
            if undirected and j not in g:
                g[j] = {}
            if weight <= 0:
                continue
            g[i][j] = weight
            if undirected:
                g[j][i] = weight
    return g


class Algorithm(IntEnum):
    uniform = 0
    astar = 1

    def __str__(self) -> str:
        if self == Algorithm.uniform:
            return "Uniform"
        elif self == Algorithm.astar:
            return "A*"
        else:
            raise ValueError("Invalid enum")


class Query(object):
    def __init__(self, alg: Algorithm, initial: int, goal: int) -> None:
        self.alg = alg
        self.initial = initial
        self.goal = goal

    def __str__(self) -> str:
        return "{} {} {}".format(str(self.alg), self.initial, self.goal)


def query_from_string(data: str) -> Query:
    str_alg, initial, goal = data.split()
    if str_alg == "A*":
        alg = Algorithm.astar  # type: Algorithm
    elif str_alg == "Uniform":
        alg = Algorithm.uniform  # type: Algorithm

    initial = int(initial)

    goal = int(goal)

    return Query(alg, initial, goal)


def res_to_result(data: Iterable[int]) -> str:
    return "-".join(map(str, data))

def stringify_graph(graph: Dict[int, Dict[int, float]]) -> str:
    result = ""
    for node, children in graph.items():
        result += "{} ->\n".format(node)
        for child, cost in children.items():
            result += "  {}: {}\n".format(child, cost)
    return result
