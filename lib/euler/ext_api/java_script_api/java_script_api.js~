/************************************************************************************
 *                             - java_script_api.java -                             *
 *                                                                                  *
 * Houses the main function of the JavaScript API, responsible for communication    *
 * between Elixir                                                                   *
 * Mix project 'euler' and problems solved in JavaScript.                           *
 ************************************************************************************/
'use strict';
/************************************************************************************
 *                                     - main -                                     *
 *                                                                                  *
 * Function responsible for fetching the problem set module specified by 'setNum',  *
 * calling its problem function specified by 'probNum', printing the results to     *
 * stdout, and handling all possible errors encountered along the way.              *
/************************************************************************************/
function main(setNum, probNum) {
  var setMod,       // module housing requested problem function
      probFunc,     // zero-arity function which returns solution
      timeStartTup, // tuple [s, ns] from abritrary time ref
      timeDiffTup,  // tuple [s, ns] time elapsed while solving problem
      solution,     // solution to requested problem
      timeElapsed;  // time elapsed (μs) rounded to the nearest integer

  function exit_on_error(message) {
    process.stderr.write('\nERROR:\n  ' + message);
    process.exit(1);
  }

  // ensure request problem set exists
  try {
    setMod = require('./set_' + setNum);
  } catch (e) {
    exit_on_error(e.message);
  }

  probFunc = 'problem' + probNum;

  // ensure requested problem function exists in requested problem set
  if (setMod[probFunc] == undefined) {
    exit_on_error('Cannot find problem ' + probNum + ' in set ' + setNum);
  }

  timeStartTup = process.hrtime();             // start timer
  solution     = setMod[probFunc]();           // call problem function
  timeDiffTup  = process.hrtime(timeStartTup); // take diff from start time

  // combine [s, ns] time tuple, converting to integer μs
  timeElapsed = Math.round((timeDiffTup[0] * 1e6)
                         + (timeDiffTup[1] / 1e3));

  // write solution to stdout, delimiting solution - time elapsed with a newline
  process.stdout.write(solution + '\n' + timeElapsed);
}

// call main function with first two unique cmd line args: 'setNum' and 'probNum'
main.apply(null, process.argv.slice(2, 4));
