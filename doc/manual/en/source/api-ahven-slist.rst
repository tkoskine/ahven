:mod:`Ahven.SList` -- Package
=============================

.. module:: Ahven.SList
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

List
''''

::

   type List is new Ada.Finalization.Controlled with private;

Cursor
''''''

::

   type Cursor is private;

Count_Type
''''''''''

::

   subtype Count_Type is Natural;

----------
Exceptions
----------

Invalid_Cursor
''''''''''''''

::
   Invalid_Cursor : exception;

List_Full
'''''''''

::

   List_Full : exception;

Thrown when the size of the list exceeds Count_Type'Last.


------------------------
Procedures and Functions
------------------------


Append
''''''

::

   procedure Append (Target : in out List; Node_Data : Element_Type);

Append an element at the end of the list.

Clear
'''''

::

   procedure Clear (Target : in out List);

Remove all elements from the list.

First
'''''

::

   function First (Target : List) return Cursor;

Return a cursor to the first element of the list.

Next
''''

::

   function Next (Position : Cursor) return Cursor;

Move the cursor to point to the next element on the list.

Data
''''

::

   function Data (Position : Cursor) return Element_Type;

Return element pointed by the cursor.

Is_Valid
''''''''

::

   function Is_Valid (Position : Cursor) return Boolean;

Tell the validity of the cursor. The cursor
will become invalid when you iterate it over
the last item.

Length
''''''

::

   function Length (Target : List) return Count_Type;

Return the length of the list.

For_Each
''''''''

::

   procedure For_Each (Target : List);

A generic procedure for walk through every item
in the list and call Action procedure for them.

