from Orchid.TypeSystem import IVariable
from Orchid.Runtime import ScriptFunction
from orchid import runtime, functions

class VariableWrapper(object):
    """ Wrapper type for instances of IVariable. """

    def __init__(self, var):
        """ Constructs a new instance of the VariableWrapper. """
        self._var = var

    def is_error(self):
        """ Returns a value indicating whether this variable represents an error. """
        return IVariable.IsError.GetValue(self._var)

    def type_code(self):
        """ Returns the native type for this variable. """
        return IVariable.TypeCode.GetValue(self._var)

    def __len__(self):
        """ Gets the length of the variable. """
        return IVariable.Length.GetValue(self._var)

    def is_excluded(self, index):
        """ Returns a value indicating whether the value at the given index is excluded. """
        return IVariable.IsExcluded(self._var, index)

    def is_included(self, index):
        """ Returns a value indicating whether the value at the given index is included. """
        return IVariable.IsIncluded(self._var, index)

    def ko_state(self, index):
        """ Ko state at the given index. """
        return IVariable.KoState(self._var, index)

    def is_auto_excluded(self, index):
        """ Returns a value indicating whether the value at the given index is automatically excluded. """
        return IVariable.IsAutoExcluded(self._var, index)

    def is_scalar(self):
        """ Returns a value indicating whether this variable represents a scalar. """
        return IVariable.IsScalar.GetValue(self._var)

    def is_vector(self):
        """ Returns a value indicating whether this variable represents a vector. """
        return IVariable.IsVector.GetValue(self._var)

    def as_string_value(self, index):
        """ Returns a string value for the given index. """
        return IVariable.AsStringValue(self._var, index)
    
    def as_double_value(self, index):
        """ Returns a double value for the given index. """
        return IVariable.AsDoubleValue(self._var, index)

    def as_bool_value(self, index):
        """ Returns a bool value for the given index. """
        return IVariable.AsBoolValue(self._var, index)

    def variable_at(self, index):
        """ Constructs a variable to represent data at the given index. """
        return VariableWrapper.wrap(IVariable.VariableAt(self._var, index))

    def unwrap(self): 
        """ Unwraps the variable. """
        return self._var

    @staticmethod
    def unwrap_all(all):
        """ Unwraps a sequence of wrapped variables, returning the result as a new list. """
        unwrapped = []
        for wrapper in all:
            unwrapped.append(wrapper.unwrap())
        return unwrapped

    @staticmethod
    def wrap(var):
        """ Wraps an instance of an IVariable. """
        return VariableWrapper(var)
    
    @staticmethod
    def wrap_all(vars):
        """ Wraps a sequence of variables into a list of wrappers"""
        wrappers = []
        for var in vars:
            wrappers.append(VariableWrapper.wrap(var))
        return wrappers

class PythonFunction(ScriptFunction):

    def __new__(cls, func, path, name, category, comment, returnTypeAsString, parameters, removeKoPoints):
        """ Constructs a new instance of a python function. """
        instance = ScriptFunction.__new__(cls, path, name, category, comment, False, "", returnTypeAsString, removeKoPoints)
        instance.func = func
        for nm, type in parameters:
            instance.AddParameter(nm, type)
        return instance

    def Execute(self, args):
        """ The main method used for calling the function that this instance wraps. """
        result = self.func(self, args)
        if isinstance(result, VariableWrapper):
            result = result.unwrap()
        return result

    def __getattr__(self, attr):
        """ Attempts to resolve unknown method calls by calling Orchid.

        This enables existing Orchid functions to be called as if they existed in the python class.  On top
        of the standard types supported by Orchid, this method can also handle python tuples and lists.

        It is therefore possible to execute Orchid functions in the following way:

        avg = self.average([1, 2, 3, 4, 5])
        stdev = self.stddev((1.98, 2.34, 3.24, 5.62))
        """
        def func(*args, **kwargs):
            return self.ExecuteExternal(runtime.Environment, attr, *args)
        return func

def build(func, name, group, parameters, return_type, source_file="", removeKoPoints=False):
    """ Responsible for the construction and registration of an Orchid function. """
    pf = PythonFunction
    pyfunc = PythonFunction(func, 
                            source_file, 
                            name, 
                            group, 
                            func.__doc__, 
                            return_type, 
                            parameters, 
                            removeKoPoints)
    functions.Add(pyfunc)

def register(function):
    """ Registers a single function with Orchid."""

    # check that function is a ScriptFunction
    if not isinstance(function, ScriptFunction):
        raise TypeError("function must extend ScriptFunction")

    functions.Add(function)

def registerFunctions(*functions):
    """ Registers each supplied function with Orchid.

    The function in this case is added to the Statistics collection
    """
    for function in functions:
        register(function)

def orchidEval(expression):
    """ This function uses Orchid to evaluate the given string expression.

    The expression must therefore be a valid Orchid calculation string.

    e.g.
    expr = "execsql('select user from dual')"
    user = orchidEval(expr)
    """
    return runtime.Evaluate(expression)
