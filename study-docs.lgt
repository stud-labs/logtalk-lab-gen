
:- object(imit_document(_Renderer_)).
   :- protected(letterRegistration/2).
      %_______________ № ________________
      %На № __________ от _______________
   letterRegistration(_AllSize, HruleSize):-
        % ::begin(tabularx,['{Size}{XX}']),
        ::underscoreFill('{18ex}'), ::run('№ '),  ::underscoreFill('{12ex}'), ::nl('0.5em'),
        ::run('На № '), ::underscoreFill(HruleSize), ::run(' от '), ::underscoreFill(HruleSize),
        %::end(tabularx).
        true.

   :- public(initAffiliationFlat/0).
   initAffiliationFlat:-
        ::cmd(noindent),
        ::begin(center),
        ::isuLogo('width=17mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное образовательное учреждение высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218, ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru}, e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
        ::end(center).

   :- public(initAffiliation/0).
   initAffiliation:-
        ::initAffiliation([]).

   :- public(initAffiliation/1).
   initAffiliation(Options):-
        ::cmd(noindent),
        ::begin(tblr, ['{width=\\linewidth, colspec={X[6]X[5]}, column{1}={c}}']),
        %::begin(center),
        ::run('{'),
        ::cmd(footnotesize),
        ::isuLogo('width=13mm'),
        ::runsLn(['МИНОБРНАУКИ РОССИИ',
                   'Федеральное государственное бюджетное',
                   'образовательное учреждение',
                   'высшего образования',
                   '«Иркутский государственный университет»',
                   '(ФГБОУ ВО «ИГУ»)',
                   'Институт математики и информационных технологий',
                   'б.~Гагарина, д.~20, Иркутск, 664003,',
                   'Тел.: (3952) 24-22-14, Факс: (3952) 24-39-63',
                   'ОКПО 02068226, ОГРН 1033801008218,',
                   'ИНН/КПП 3808013278/380801001',
                   '\\texttt{http://www.math.isu.ru},',
                   'e-mail: \\texttt{ime@math.isu.ru}']),
        ::vspace('0.7em'),
        ::letterRegistration('25ex','12ex'),
        %::end(center),
        ::run('}'),
        ::tab,
        %(::option(center(Center), Options) -> call(Center); true),
        %::tab,
        (::option(right(Right), Options) -> call(Right); true),
        ::end(tblr).

:- end_object.


:- object(person(_Alias_, _Name_, _BirthDate_)).
:- end_object.

:- object(student(_Alias_, _Name_, _Gender_, _BirthDate_),
   extends(person(_Alias_, _Name_, _BirthDate_))).
:- end_object.

:- object(instructor(_Alias_, _Name_, _BirthDate_,
                     _Degree_, _AcademicTitle_, _Position_),
   extends(person(_Alias_, _Name_, _BirthDate_))).
:- end_object.

:- protocol(catalogp).
   :- protected(el/2).  % el(<Alias, Symbol>,<Structure>).
   :- protected(el/1).  % el(<Structure>). % No name/alias
   :- public(name/1).   % Name of catalog
   :- public(element/1).
   :- public(element/2).
:- end_protocol.

:- protocol(departmentp,
   extends(catalogp)).
   :- public(head/1).% Alias of department head person.
:- end_protocol.

:- protocol(curriculump).
   :- public(course/7).
   :- public(departments/1).
   :- public(currentStudyYear/1).
   :- public(startDate/1).
   :- public(endDate/1).
   :- public(enrollOrder/2).
   :- public(specialty/2).
   :- public(institute/1).
   :- public(faculty/1).
   :- public(instructor/2).
:- end_protocol.

:- protocol(notesp).
   :- public(notes/2).
:- end_protocol.

:- category(curriculumc,
   implements(curriculump)).

   :- use_module(library(lists), [member/2]).
   instructor(Alias, Person):-
       ::departments(Departments),
       member(Department, Departments),
       Department::element(Alias, Person).
       % arg(1, Person, Alias).

:- end_category.

:- object(studentGroup(_Name_),
   implements(catalogp)).
   name(_Name_).
   element(Element) :-
       ::el(Element).
   element(Alias, Element) :-
       ::el(Alias, Element).
:- end_object.

:- object(course(_Alias_, _Functor_)).
:- end_object.

:- object(department,
   implements(catalogp)).
   name('ERROR: Забыл задать название подразделения').
   element(Element) :-
       ::el(Element).
   element(Alias, Element) :-
       ::el(Alias, Element).
   element(Alias, Element) :- % Search in "subdivisions".
       ::el(Compound),
       current_object(Compound),
       Compound::element(Alias, Element).
:- end_object.

:- object(chair,
   extends(department)).
   name('ERROR: Забыл задать название кафедры').
:- end_object.

:- category(notesc).
   :- public(translate/2).
   translate(2, 'неудовл.').
   translate(3, 'удовл.').
   translate(4, 'хорошо').
   translate(5, 'отлично').
   translate(no, 'незачет').
   translate(ok, 'зачтено').
   translate(abs, 'неявка').
:- end_category.

:- category(signaturec(_Renderer_, _Curriculum_, _SignPerson_)).
   % Генератор подписи, наример, директора
   % importing this category implies importing category optionc.

   :- public(signature/2).
   :- meta_predicate(signature(0, 0)).
   signature(Before, After):-
        % Before is a GOAL, запускается ДО изображения подписи.
        % After is a GOAL, запускается ПОСЛЕ изображения подписи.
        R = _Renderer_,
        _Curriculum_::instructor(_SignPerson_, Person),
        Person = teacher(PersonName, _, Degree, AcademicPos, Roles),
        (::option(man(Management), Roles) ->
            format(atom(SignatureAffil), '~w, ~w, ~w', [Management, Degree, AcademicPos]);
            format(atom(SignatureAffil), '~w, ~w', [Degree, AcademicPos])
        ),
        (::option(sign(SignName), Roles) ->
            SignNameString=SignName;
            SignNameString=PersonName
        ),
        call(Before),
        R::cmd(noindent),
        R::begin(tblr,['{width=1\\linewidth, colspec={X[3]XX[2]}, column{1}={l}, column{3}={r}}']),
        R::run('~w',[SignatureAffil]), R::tab, R::tab, R::run(SignNameString),
        R::end(tblr),
        call(After).

   :- public(signature/0).
   signature:-
        R = _Renderer_,
        ::signature((R::vfill,R::vfill),(R::vfill,R::strut)).

:- end_category.

%% Group 02223

:- object(chairIT,
   extends(chair)).
   name('информационных технологий').
   el(cea, teacher('Черкашин Евгений Александрович', 1974-19-08, 'канд. техн. наук', 'доцент', 'доцент')).
   el(mih, teacher('Михайлов Андрей Анатольевич', unknown, 'канд. техн. наук', 'доцент', 'доцент')).
   el(kom, teacher('Комаров Алексей Владимирович', unknown, 'канд. техн. наук', 'доцент', 'доцент')).
:- end_object.

:- object(chairFL,
   extends(chair)).
   name('иностранных языков').
   el(pan, teacher('Панченко Наталья Николаевна', unknown, 'канд. филол. наук', 'доцент', 'доцент')).
:- end_object.

:- object(chairPh,
   extends(chair)).
   name('философии').
   el(br, teacher('Бранденбург Владимир Яковлевич', unknown, 'д-р филос. наук', 'профессор', 'профессор')).
:- end_object.

:- object(chairVMO,
   extends(chair)).
   name('вычислительной математики и оптимизации').
   el(arg, teacher('Аргучинцев Александр Валерьевич', unknown, 'д-р физ.-мат. наук', 'профессор', 'профессор')).
:- end_object.

:- object(chairPT,
   extends(chair)).
   name('теории вероятностей и дискретной математики').
   el(kol, teacher('Колокольникова Наталья Арсеньевна', unknown, 'канд. физ.-мат. наук', 'доцент', 'доцент')).
:- end_object.

:- object(chairECM,
   extends(chair)).
   name('вычислительной техники').
   el(ponm, teacher('Пантелеев Михаил Георгиевич', unknown, 'канд. техн. наук', 'доцент', 'доцент')).
   el(leb, teacher('Лебедев Сергей Вячеславович', unknown, '', 'старший преподаватель', 'старший преподаватель')).
:- end_object.

:- object(directionIMIT,
   extends(department)).
   name('дирекция ИМИТ').
   el(fal, teacher('Директор Иван Иванович', unknown, 'д-р~физ.-мат.~наук', 'профессор',
                   [man='Директор ИМИТ ИГУ', edu='профессор', sign="И.~И.~Директор"])).
:- end_object.

:- object(instituteIMIT,
   extends(department)).
   name('математики и информационных технологий').  % Пока нет реализации склонений русского языка
   el(directionIMIT).
   el(chairFL).
   el(chairPT).
   el(chairIT).
:- end_object.

:- object(universityISU,
   extends(department)).
   name('Иркутский государственный университет').
   el(instituteIMIT).
   el(chairPh).   % TODO: Pack into a corresponding institute/faculty
:- end_object.

:- object(etu,
   extends(department)).
   name('Санкт-Петербургский государственный электротехнический университет «ЛЭТИ» им. В.И. Ульянова (Ленина)').
   el(chairECM).
:- end_object.

:- object(curriculum2023,
   implements(curriculump),
   imports(curriculumc)).

   departments([universityISU,etu]).  % Или можно создать 'виртуальный' университет
   currentStudyYear(2).
   startDate(2022-09-01).
   endDate(2024-08-31).
   enrollOrder(4459/3, 2022-08-21).
   institute('математики и информационных технологий').
   specialty('01.04.02',
     'Прикладная математика и информатика, профиль «Семантические технологии и многоагентные системы» (уровень магистратуры)').

   %      code,  name,                               days,sem, kind, date final, instructor
   course(akm, course('Алгоритмы компьютерной математики'),       180/5, 1, exam,  2023-01-12, cea).
   course(is,  course('Интеллектуальные системы'),                108/4, 1, exam,  2023-01-18, cea).

   course(fl1,  course('Иностранный язык в профессиональной деятельности (семестр 1)'),
                                                                   72/2, 1, cred,  2022-12-05, panch).
   course(com, course('Коммерциализация результатов научно-исследовательских разработок'),
                                                                   72/2, 1, credN, 2022-12-10, kom).
   course(cmm, course('Непрерывные математические модели'),       144/4, 1, credN, 2022-12-16, cea).
   course(tpc, course('Теория и практика межкультурных коммуникаций'),
                                                                   72/2, 1, cred,  2022-12-06, br).
   course(iot, course('Интернет вещей'),                           72/2, 1, credN, 2022-12-16, cea).
   course(ddb, course('Распределенные базы данных'),               72/2, 1, credN, 2022-12-16, cea).
   course(ml,  course('Машинное обучение'),                        72/2, 1, credN, 2022-12-09, mih).
   course(spr, course('Учебная (технологическая (проектно-технологическая)) практика'),
                                                                  108/3, 1, credN, 2022-12-22, cea).

   % Semester 2
   course(dis, course('Проектирование информационных систем на основе семантических технологий'),
                                                                  180/5, 2, exam,  2023-06-23, leb).
   course(sw,  course('Семантический Веб'),                       180/5, 2, exam,  2023-06-19, ponm).
   course(ssp, course('Статистика случайных процессов'),          144/4, 2, exam,  2023-06-14, kol).

   course(ais, course('Аналитические информационные системы'),     72/2, 2, credN, 2023-06-09, cea).
   course(fl2, course('Иностранный язык в профессиональной деятельности (семестр 2)'),
                                                                   72/2, 2, cred,  2023-06-07, panch).
   course(rec, course('Математичсекие методы распознавания образов'),
                                                                   144/4,2, credN, 2023-06-09, cea).
   course(rnd, course('Управление исследовательской деятельностью'),
                                                                   72/2, 2, cred,  2023-06-09, arg).
   course(nn,  course('Нейронные сети'),                           72/2, 2, credN, 2023-06-09, cea).
   course(ds,  course('Разработка приложений в распределенной среде'),
                                                                   72/2, 2, credN, 2023-06-09, cea).
   course(dda, course('Анализ распределенных данных'),             72/2, 2, credN, 2023-06-09, cea).


   course(kp1, course('Курсовая работа, Проектирование информационных систем на основе семантических технологий'),
                                                                  none,  2, credN, 2022-06-09, cea).
   course(ppr, course('Производственная практика (научно-исследовательская работа)'),
                                                                  108/3, 1, credN, 2023-07-10, cea).
   course(predp, course('Производственная практика, преддипломная'),
                                                                  972/27, 2, credN, 2024-06-2, cea).
:- end_object.

:- object(notesDocument(_Renderer_, _Notes_, _Student_, _Curriculum_, _SignPerson_),
   imports([notesc, russianc, optionc,
            signaturec(_Renderer_, _Curriculum_, _SignPerson_)]),
   extends(imit_document(_Renderer_)),
   implements(documentp)).
   :- use_module(library(lists), [member/2]).

   gen:-
        R = _Renderer_,
        ::initAffiliationFlat,
        R::begin(center),
        ::subjectDefinition,
        R::end(center),
        R::emptyLine,
        ::tableHeader,
        forall(member(Note, _Notes_), ::tableRow(Note)),
        ::tableFooter,
        ::statistics,
        ::signature,
        true.

/*

Справка об успеваемости за весь период обучения:
Выдана _____________________________________________________-
в том, что он является студентом ___ курса очной формы обучения, обучается по направлению подготовки (специальности) высшего образования _______________________ факультета(института)_______________________

*/

   :- protected(statistics/0).
   statistics:-
        R = _Renderer_,
        ::statistics([5,4,ok], Total),
        ::statistics([5], Fives),
        ::statistics([4], Goods),
        R::cmd(noindent),
        R::run('Общее количество оценок -- ~w', [Total]), R::nl,
        R::run('Количество оценок "Отлично" -- ~w', [Fives]), R::nl,
        R::run('Количество оценок "Хорошо" -- ~w', [Goods]),
        true.

   :- protected(statistics/2).
   statistics([], 0).
   statistics([Var|T], Sum):-
        findall(1, member(_-Var,_Notes_), L),
        length(L, Sum1),
        statistics(T, Sum2),
        Sum is Sum1 + Sum2.

   subjectDefinition:-
        _Renderer_ = R,
        R::run('{'),
        R::cmd([large,bfseries,sffamily]), R::run('Справка об успеваемости за весь период обучения'),
        R::run('}'), R::nl('1em'),
        R::run('Выдана '),
        _Student_ = student(_Alias, PersonNames, Gender, _),
        ::option(dat(PersonName), PersonNames),
        R::run('\\textbf{~w}',[PersonName]), % R::nl,
        R::run(' в том, что '),
        ::choice(Gender, [f='она', m='он'], Pronoun),
        R::run(Pronoun),
        R::run(' является студентом '),
        C = _Curriculum_,
        C::currentStudyYear(StudyYear),
        R::run(' {~w} ', [StudyYear]),
        R::run(' курса магистратуры очной формы обучения, обучается по направлению подготовки (специальности) высшего образования '),
        C::specialty(Code, Name),
        R::run(' {\\bfseries ~w~~~w  ', [Code, Name]),
        (C::institute(InstituteName)->
          R::run(' Института '),
          R::run(InstituteName); true),
        (C::faculty(FacultyName)->
          R::run(' факультета '),
          R::run(FacultyName); true),
        R::eenv,
        R::run('{\\bfseries ФГБОУ ВС <<ИГУ>>}'),
        true.


   :- protected(tableHeader/0).
   tableHeader:-
        _Renderer_ = R,
        R::cmd('setcounter{mytableline}{0}'),
        ::longtblrStyle(default),
        R::begin(longtblr,[
            '[caption={}]',
            '{',
            'width=1\\linewidth,rowhead=1,colspec={|X|X[15]|X[3]|}, row{1} = {c}, column{1} = {r}, column{3} = {c}, hlines}'
            ]),
        R::run(' \\textbf{№} '), R::tab,
        R::run(' \\textbf{Дисциплина} '), R::tab,
        R::run(' \\textbf{Оценка} '), R::nl,
        ::resetLineNumber.

   :- protected(tableFooter/0).
   tableFooter:-
        _Renderer_ = R,
        R::end(longtblr).

   :- protected(tableRow/1).
   tableRow(Subject-Note) :-
        ::translate(Note, NoteName),
        _Renderer_ = R,
        ::incNumber(LineNumber),
        R::run(LineNumber), R::tab,
        _Curriculum_::course(Subject, course(SubjectName), _, _, _, _, _),
        R::run(SubjectName), R::tab,
        R::run(NoteName), R::nl.

   :- protected(resetLineNumber/0).
   resetLineNumber:-
        retractall(lineNumber(_)),
        assertz(lineNumber(0)).

   :- private(lineNumber/1).
   :- dynamic(lineNumber/1).

   :- protected(incNumber/1).
   incNumber(Number):-
        lineNumber(Num),
        Number is Num + 1,
        retractall(lineNumber(_)),
        assertz(lineNumber(Number)).

:- end_object.


:- object(referenceDocument(_Renderer_,_Student_, _Curriculum_, _SignPerson_),
   imports([notesc, russianc, optionc,
            signaturec(_Renderer_, _Curriculum_, _SignPerson_)]),
   extends(imit_document(_Renderer_)),
   implements(documentp)).

   :- use_module(library(lists), [member/2]).

   gen:-
        R = _Renderer_,
        self(Self),
        ::initAffiliation([right=Self::right]),
        % R::emptyLine,
        R::begin(center),
        R::run('Справка дана для предъявления {\\bfseries по месту требования}'),
        R::end(center),
        % ::signature,
        ::signature(
            (
             R::par,
             % R::strut,
             R::vspace('2em')
            ),
            (
             R::par,
             R::emptyLine,
             R::cuttingLine,
             R::vfill,
             R::strut
            )
        ),
        true.

   :- public(right/0).
   right:-
        ::subjectDefinition.

   % :- use_module(library(option), [option/2 as locoption]).

   subjectDefinition:-
        _Renderer_ = R,
        R::run('{'),
        R::cmd([bfseries,sffamily,centering]), R::run('СПРАВКА'), R::par,
        R::run('}'),
        R::benv, %R::cmd([small]),
        R::cmd('hspace{\\parindent}'),
        R::run('Дана '),
        _Student_ = student(_Alias, PersonNames, Gender, Birth),
        ::option(dat(PersonName), PersonNames),
        R::run('\\textbf{~w}',[PersonName]), % R::par,
        R::date(Birth),
        R::run(' г.р., '),
        R::run(' в том, что '),
        ::choice(Gender, [f='она', m='он'], Pronoun),
        R::run(Pronoun),
        R::run(' действительно является студентом '),
        C = _Curriculum_,
        C::currentStudyYear(StudyYear),
        R::run('{\\bfseries ~w} курса магистратуры ', [StudyYear]),
        R::benv,
        R::cmd(bfseries),
        (C::institute(InstituteName)->
          R::run(' Института '),
          R::run(InstituteName); true),
        (C::faculty(FacultyName)->
          R::run(' факультета '),
          R::run(FacultyName); true),
        R::run('ФГБОУ ВС <<ИГУ>>'),
        R::eenv,
        R::run(' очной формы обучения (за счет бюджетных ассигнований федерального бюджета) '),
        R::run(' по основной образовательной программе по направлению '),
        C::specialty(Code, Name),
        R::run(' {\\bfseries ~w~~~w } ', [Code, Name]),
        %R::run('(уровень магистратуры)'),
        R::run(' с '),
        C::startDate(StartDate),
        R::date(StartDate),
        C::enrollOrder(OrderNumber, EnrollDate),
        R::date(EnrollDate, EnrollDateString),
        R::run(' (Приказ на зачисление ~w от ~w)', [OrderNumber, EnrollDateString]),
        C::endDate(EndDate),
        R::date(EndDate, EndDateString),
        R::run(' Предполагаемый срок окончания обучения: ~w.  ', [EndDateString]),
        R::eenv,
        true.

:- end_object.


:- object(potaninDocuments(_Renderer_, _Notes_, _StudentGroup_, _Curriculum_, _SignPerson_),
   extends(documents(_Renderer_)),
   imports(notesc)).

   :- public(gen/0).
   gen:-
        ::start,
        forall((_Notes_::notes(Person, Notes),
                _StudentGroup_::element(Person, student(PersonName, Gender, PersonAge))),
               (
                _Renderer_::newpage,
                notesDocument(_Renderer_, Notes,
                    student(Person, PersonName, Gender, PersonAge),
                    _Curriculum_, _SignPerson_
                )::gen,
                _Renderer_::newpage,
                referenceDocument(_Renderer_,
                    student(Person, PersonName, Gender, PersonAge),
                    _Curriculum_, _SignPerson_
                )::gen,
                % _Renderer_::cuttingLine,
                % format('~w\n', [student(Person, PersonName, Gender, PersonAge)]),
                true)
        ),
        ::end,
:- end_object.
