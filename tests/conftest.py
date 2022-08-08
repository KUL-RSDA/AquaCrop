def pytest_addoption(parser):
    parser.addoption(
        '--omit_listprojects', action="store_true", default=False,
        help="Do not create the ListProjects.txt input file")
