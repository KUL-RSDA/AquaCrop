import os
import shutil
import tempfile
import time
import pytest
import numpy as np


def is_within_one_significant_digit(str_ref, str_val):
    """
    Returns whether two floating point numbers (given as strings)
    have at most a unit difference in the least significant digit
    (see the test_digits_check() test).
    """
    assert len(str_ref) == len(str_ref.strip()), str_ref
    assert len(str_val) == len(str_val.strip()), str_val

    if '.' in str_ref:
        assert '.' in str_val, (str_ref, str_val)
        # Check that both have same number of digits after the decimal
        num_ref = len(str_ref) - str_ref.index('.') - 1
        num_val = len(str_val) - str_val.index('.') - 1
        assert num_ref == num_val, (str_ref, str_val)
    else:
        assert '.' not in str_val, (str_ref, str_val)

    int_ref = int(str_ref.replace('.', '', 1))
    int_val = int(str_val.replace('.', '', 1))
    is_ok = abs(int_ref - int_val) < 2
    return is_ok


def myassert(condition, message, root_dir, logger):
    """
    Wrapper for the 'assert' function, where the current working
    directory is changed before raising the AssertionError.
    """
    if not condition:
        logger.print('ERR:', message)
        os.chdir(root_dir)
    assert condition, message
    return


class Logger:
    def __init__(self, filename):
        self.fh = open(filename, 'a')

    def print(self, *args, **kwargs):
        print(*args, file=self.fh, **kwargs)
        return

    def __del__(self):
        self.fh.close()


@pytest.mark.parametrize('param', [
    ('09', '10', True),
    ('9', '10', True),
    ('9.9', '10.0', True),
    ('14.9', '15.0', True),
    ('15.361', '15.362', True),
    ('15.359', '15.360', True),
    ('15.300', '15.299', True),
    ('90', '99', False),
    ('9.0', '10.0', False),
    ('14.9', '15.1', False),
    ('15.361', '15.363', False),
    ('15.371', '15.361', False),
])
def test_digits_check(param):
    log = Logger('log_europe_digits.txt')
    str_ref, str_val, expected = param
    result = is_within_one_significant_digit(str_ref, str_val)
    is_ok = result == expected
    log.print('Checking {0} vs {1}: {2} == {3} = {4}'.format(
                 str_ref, str_val, expected, result, 'OK' if is_ok else 'FAIL'))
    assert is_ok, (str_ref, str_val, expected, result)
    return


@pytest.mark.parametrize('row', range(51))
@pytest.mark.parametrize('col', range(105))
@pytest.mark.parametrize('use_irrigation', [False, True])
def test_europe(row, col, use_irrigation):
    filename = 'log_europe_{0}-{1}-{2}.txt'.format(use_irrigation, col, row)
    log = Logger(filename)

    log.print('=========', row, col, use_irrigation, '=========')
    cwd = os.getcwd()

    # AquaCrop executable
    executable = os.environ['AQUACROP_COMMAND']

    # Location of the INPUT folder
    test_dir = os.path.join(os.environ['AQUACROP_TEST_ROOT'], 'test_europe')
    input_dir = os.path.join(test_dir, 'INPUT')

    pixel_name = str(row) + '_' + str(col)

    # Check that a soil file exists
    soil_dir = os.path.join(input_dir, 'soil_files')
    soil_name = str(row) + '_' + str(col) + '.SOL'
    soil_file = os.path.join(soil_dir, soil_name)
    if not os.path.exists(soil_file):
        log.print('SKIP: missing soil file for {0}: {1}'.format(pixel_name,
                                                                soil_file))
        return

    # Create a temporary working directory for the current test
    work_dir = tempfile.mkdtemp(dir=cwd)

    irrigation_suffix = 'Irr' if use_irrigation else 'noIrr'

    output_dir = os.path.join(work_dir, 'OUTPUT_' + irrigation_suffix)
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)

    # Make new local directory for each pixel
    pixel_dir = os.path.join(output_dir, pixel_name)
    if not os.path.exists(pixel_dir):
        os.mkdir(pixel_dir)
    os.chdir(pixel_dir)

    # Place Aquacrop directories SIMUL and LIST in local directory
    d = os.path.join(pixel_dir, 'SIMUL')
    if os.path.exists(d):
        shutil.rmtree(d)
    shutil.copytree(os.path.join(input_dir, 'SIMUL'), d)

    for d in [os.path.join(pixel_dir, 'OUTP'), os.path.join(pixel_dir, 'LIST')]:
        if not os.path.exists(d):
            os.mkdir(d)

    # Copy the PRM file to the temporary LIST directory
    prm_name = '{0}.PRM'.format(pixel_name)
    prm_file_inp = os.path.join(input_dir, 'prm_templates', irrigation_suffix,
                                prm_name)
    prm_file_out = os.path.join(pixel_dir, 'LIST', prm_name)
    shutil.copyfile(prm_file_inp, prm_file_out)

    # Enclose all directory paths of the PRM file in quotes
    # For this to work properly, we first need to replace any DOS-style
    # line endings to UNIX-style line endings
    cmd = 'sed -i "s/\r//" {0}'.format(prm_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__INPUTDIR__.*@\"&\"@\' {0}'.format(prm_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__OUTPUTDIR__.*@\"&\"@\' {0}'.format(prm_file_out)
    os.system(cmd)
    # Now fill in the template paths
    cmd = 'sed -i "s@__INPUTDIR__@{0}@g" {1}'.format(input_dir, prm_file_out)
    os.system(cmd)
    cmd = 'sed -i "s@__OUTPUTDIR__@{0}@g" {1}'.format(output_dir, prm_file_out)
    os.system(cmd)

    listprojects_file = os.path.join(pixel_dir, 'LIST', 'ListProjects.txt')
    with open(listprojects_file, 'w') as f:
        f.write(prm_name + '\n')

    # Run Aquacrop
    cmd = os.path.join(executable)
    t_start = time.time()
    exitcode = os.system(cmd)
    t_stop = time.time()
    log.print('Runtime: {0:.1f} seconds'.format(t_stop - t_start))
    myassert(exitcode == 0,
             'AquaCrop exited with exit code {0}'.format(exitcode),
             cwd, log)

    # Remove extra files and directories: only save PRM and output file
    shutil.rmtree(os.path.join(pixel_dir, 'SIMUL'))
    shutil.rmtree(os.path.join(pixel_dir, 'LIST'))
    os.remove(os.path.join(pixel_dir, 'OUTP', 'AllDone.OUT'))
    os.remove(os.path.join(pixel_dir, 'OUTP', 'ListProjectsLoaded.OUT'))

    # Check that the output is the same as the reference
    for suffix in ['day', 'season']:
        filename = '{0}PRM{1}.OUT'.format(pixel_name, suffix)

        reference_file = os.path.join(test_dir, 'OUTPUT_' + irrigation_suffix,
                                      pixel_name, 'OUTP', filename)
        with open(reference_file, 'r', encoding='ISO-8859-1') as f:
            ref_lines = f.readlines()

        output_file = os.path.join(output_dir, pixel_name, 'OUTP', filename)
        with open(output_file, 'r', encoding='ISO-8859-1') as f:
            out_lines = f.readlines()

        myassert(len(ref_lines) == len(out_lines), \
                 ('Different number of lines in output and reference files ',
                  reference_file, output_file),
                 cwd, log)

        num_col = {'day': 114, 'season': 41}[suffix]
        num_not_realclose = [0] * num_col
        num_items_total = [0] * num_col

        for i, (ref_line, out_line) in enumerate(zip(ref_lines, out_lines)):
            if i == 0:
                # Only elementary checks for the header line
                myassert(ref_line.startswith('AquaCrop'),
                         (i, ref_line, out_line), cwd, log)
                myassert(out_line.startswith('AquaCrop'),
                         (i, ref_line, out_line), cwd, log)
                myassert('Output created on' in out_line,
                         (i, ref_line, out_line), cwd, log)
                myassert('Output created on' in ref_line,
                         (i, ref_line, out_line), cwd, log)
            else:
                items_ref = ref_line.split()
                for i in range(len(items_ref)):
                    num_items_total[i] += 1

                try:
                    assert ref_line == out_line, (i, ref_line, out_line)
                except AssertionError:
                    log.print('WARN: need item-by-item check (line {0})'.format(i))
                    items_out = out_line.split()

                    for icol, (item_ref, item_out) in enumerate(zip(items_ref,
                                                                    items_out)):
                        if item_ref != item_out:
                            is_realclose = np.isclose(float(item_ref),
                                                      float(item_out))
                            if not is_realclose:
                                num_not_realclose[icol] += 1

                                try:
                                    is_close = is_within_one_significant_digit(
                                                            item_ref, item_out)
                                except AssertionError:
                                    myassert(False, ('failure checking digits',
                                             icol, item_ref, item_out), cwd, log)
                                else:
                                    myassert(is_close, (icol, item_ref,
                                             item_out), cwd, log)


        log.print('{0} num_items_total = {1}'.format(filename,
                                                   sum(num_items_total)))
        log.print('{0} num_not_realclose = {1}'.format(filename,
                                                     sum(num_not_realclose)))
        numdevlist = [i * 100. / j for (i, j) in zip(num_not_realclose,
                                                     num_items_total)]
        log.print('{0} numdevlist [%] = {1}'.format(filename,
                        ' '.join(map(lambda x: '%.3f' % x, numdevlist))))
        maxdev = np.max(numdevlist)
        maxcol = np.argmax(numdevlist)
        log.print('{0} maxnumdev [%] = {1:.3f} (column index {2})'.format(
                                                    filename, maxdev, maxcol))

        # Now we check that the number of small deviations are within bounds.
        # Generally, no such small deviations are allowed. But with irrigation,
        # a few pixels are known to display small deviations in *PRMday.out.

        # Make a dictionnary for the number of allowed small deviations
        # per special case
        special_cases = {
            (30, 20, True, 'day'):  1,
            (50, 27, True, 'day'):  1,
            (7, 39, True, 'day'):   2,
            (0, 45, True, 'day'):   1,
            (18, 59, True, 'day'):  1,
            (18, 60, True, 'day'):  2,
            (13, 63, True, 'day'):  1,
            (12, 64, True, 'day'):  1,
            (39, 8, True, 'day'):   1,
        }
        if (row, col, use_irrigation, suffix) in special_cases:
            tolerated_deviations = special_cases.get((row, col, use_irrigation, suffix))
            msg = '{0} small deviation(s) currently allowed for this test case'
            log.print(msg.format(tolerated_deviations))
            myassert(int(np.sum(num_not_realclose)) <= tolerated_deviations,
                     'more than allowed number of small deviation detected for a special case',
                     cwd, log)
        else:
            myassert(np.all(num_not_realclose) == 0,
                     'small deviations detected for a non-special case',
                     cwd, log)

        log.print('{0} checks = OK'.format(filename))

    os.chdir(cwd)
    shutil.rmtree(work_dir)
