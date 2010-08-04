:mod:`Ahven` -- Package
=======================

.. module:: Ahven
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

----------
Exceptions
----------

.. _assertion-error: ahven-assertion_error

**Assertion_Error**

    Exception, raised when Assert fails.

------------------------
Procedures and Functions
------------------------

Assert
''''''

::

    procedure Assert (Condition : Boolean; Message : String);

If Condition is false, Assert raises Assertion_Error
with given Message.


Assert_Equal
''''''''''''

::

    generic
       type Data_Type is private;
       with function Image (Item : Data_Type) return String is <>;
    procedure Assert_Equal (Actual : Data_Type; Expected : Data_Type; Message : String);


If Expected /= Actual, Assert raises Assertion_Error
with given Message.

Fail
''''

::

    procedure Fail (Message : String);

Fail always raises Assertion_Error with given Message.

