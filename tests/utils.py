import os
import tempfile


def create_working_directory():
    """ Creates a working directory with a random name. """

    # Choose location of the directory
    if 'AQUACROP_TMPDIR' in os.environ:
        tmpdir = os.environ['AQUACROP_TMPDIR']
    else:
        # $TMPDIR will be used in this case
        tmpdir = None

    # Specify suffix to make it more recognizable
    suffix = '_aquacrop_' + os.environ['USER']
    work_dir = tempfile.mkdtemp(dir=tmpdir, suffix=suffix)
    return work_dir
