# `Expression`
Выражение - смысловая единица рудиментов. Данные, типы, сообщения, навыки, агенты - все это выражения.

У выражения могут быть аргументы и выходные значения.

# `Predicate`
Специальное выражение, которое возвращает единственное значение с типом Bool: `true` или `false`.

## Field, Type, Structure
Тип - структура, описывающая поля и их типы, рекурсивно.

Если рассматривать тип как выражение, то это композитный предикат, принимающий на вход данные:
* `ID` - идентификатор поля внутри типа. Вложенные поля - допустимы и разрешены без создания типа.
* `Type`
  * один из примитивных типов
  * `ID` типа
* дополнительные требования структуры - ∀, ∃, ∄ и т.п. проверяющие состав списков

## `OneOf`, `AnyOf`, `AllOf`
Типы сами по себе часто участвуют в выражениях для логического разделения ветвей алгоритма, т.е. с каждым предикатом ассоциирован набор выражений P -> E.

Варианты композиции:
* `OneOf(P1, P2, P3...)` - для предложенных данных выполнится только одна ветвь. Порядок - важен. Эквивалент match-case.
* `AnyOf(P1, P2, P3...)` - каждая ветвь, подходящая по предикату - выполнится. Порядок - не важен.
* `AllOf(P1, P2, P3...)` - все предикаты должны выполниться. Порядок - не важен.

Тип - это пример предиката `AllOf`, весь набор предикатов полей обязан выполниться на данных, чтобы тип как выражение вернул `true`.

# Data
Данные в рудиментах всегда сопровождаются предикатом, который выполняется для конкретного набора данных.

Предикат данных позволяет, не обращаясь к самим данным, управлять потоком управления.