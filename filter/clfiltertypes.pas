unit CLFilterTypes;

{$mode objfpc}{$H+}

interface

const
  filter_maxcount = 20;
  filter_operators: array[0..6] of string =
    ('< %s', '<= %s', '= %s', '>= %s', '> %s', 'LIKE %s', 'LIKE %s');
  filter_contentleft: array[0..6] of string =
    ('',     '',      '',     '',      '',     '',        '%');
  filter_contentright: array[0..6] of string =
    ('',     '',      '',     '',      '',     '%',       '%');
  filter_captions: array[0..6] of string =
    ('Меньше', 'Меньше или равно', 'Равно', 'Больше или равно', 'Больше', 'Начинается с', 'Содержит');

type
  TFilterState = Record
    count: integer;
    field: array [0..filter_maxcount-1] of integer;
    oper: array [0..filter_maxcount-1] of integer;
    content: array [0..filter_maxcount-1] of string;
  end;

implementation

end.

