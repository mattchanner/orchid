from orchid_api import build, VariableWrapper
import re

def if_error(orchid, args):
    """ Returns value_if_error if expression is an error and the value of the expression itself otherwise. """
    val, default = VariableWrapper.wrap_all(args)

    res = map(lambda x: default if x.is_error() else x, [val.variable_at(i) for i in xrange(len(val))])
    
    # return a scalar if that was what we were given
    return res[0].unwrap if len(res) == 1 else VariableWrapper.unwrap_all(res)

build(
    name = "IFERROR",
    func = if_error,
    group = "Logical",
    parameters = (("expression", "IVariable"), ("value_if_error", "IVariable")),
    return_type = "IVariable",
    source_file = __file__
)

def re_match(orchid, args):
    """ Applies the regular expression pattern at the start of the string, returning true if a match is found.
        Usage:
            regexMatch({pattern}, {string})
    """
    pattern, string = args
    return re.match(pattern, string) != None

build(
    name = 'RegexMatch',
    group = 'String Functions',
    func = re_match,
    parameters = (("pattern", "String"), ("string", "String")),
    return_type = 'Boolean',
    source_file = __file__
)

def re_find_all(orchid, args):
    """ Returns a list of all non-overlapping matches in a string based on a regular expression
        Usage:
        regexFindAll({pattern}, {string})
    """
    pattern, string = args
    matches = re.findall(pattern, string)
    result = []
    if matches:
        for match in matches:
            result.append(match)
    return result

build(
    name = 'RegexFindAll',
    group = 'String Functions',
    func = re_find_all,
    parameters = (("pattern", "String"), ("string", "String")),
    return_type = 'String[]',
    source_file = __file__
)

def re_find(orchid, args):
    """ Returns the first match in a string based on a regular expression
        Usage:
        regexFind({pattern}, {string})

        Example (extract the value inside the parenthesis):
        <code>regexfind("(?<=\().*(?=\))", "289.0 (345)") </code>
        Returns:
        '345'
    """
    pattern, searchStr = args
    match = re.search(pattern, searchStr)
    return match.group() if match else ""

build(
    name = 'RegexFind',
    group = 'String Functions',
    func = re_find,
    parameters = (("pattern", "String"), ("string", "String")),
    return_type = 'String[]',
    source_file = __file__
)