/**
 * Created by Moan on 06/05/16.
 */


/**
 * Checks if a given value is an integer
 * @param value
 * @returns {boolean}
 */
function isInt(value) {
    if (isNaN(value)) {
        return false;
    }
    var x = parseFloat(value);
    return (x | 0) === x;
}

