/* CMPS 112 - Assignment 5
Michael Zhang mzhang62@ucsc.edu
Haofan Wang hwang108@ucsc.edu
*/

%NOT operation 
%helper function, provided by professor
not( X ) :- X, !, fail.
not( _ ).

%convert degreeMin to degree
degreeMin_to_degree(degmin(Degrees, Minutes), Result_degrees) :-
    Result_degrees is Degrees + Minutes / 60.

%convert degree to radian, need to first convert degreeMin to degree
degree_to_radian(Input_degree, Output_radian) :-
    Output_radian is Input_degree * pi / 180.

%use the haversine formula to compute the distance between two coordinates
haversine_formula_distance(Lat1, Long1, Lat2, Long2, Result_distanace) :-
    %first convert each components to radian
    degreeMin_to_degree(Lat1, Lat1_degree),
    degreeMin_to_degree(Long1, Long1_degree),
    degreeMin_to_degree(Lat2, Lat2_degree),
    degreeMin_to_degree(Long2, Long2_degree),
    degree_to_radian(Lat1_degree, Lat1_radian),
    degree_to_radian(Long1_degree, Long1_radian),
    degree_to_radian(Lat2_degree, Lat2_radian),
    degree_to_radian(Long2_degree, Long2_radian),
    %haversine formula
    Dlon is Long2_radian - Long1_radian,
    Dlat is Lat2_radian - Lat1_radian,
    A is sin(Dlat / 2) ** 2 + cos(Lat1_radian) * cos(Lat2_radian)
     * sin(Dlon / 2) ** 2,
    C = 2 * atan2(sqrt(A), sqrt(1 - A)),
    % 3961 is Earth's radius in miles
    %result is in mules
    Result_distanace is 3959 * C.


%compute the distance between two airports
airports_distance(Airport1, Airport2, Airport12_Distance) :-
     %find the airport in the database
    airport(Airport1, _, Lat1, Long1),
    airport(Airport2, _, Lat2, Long2),
    haversine_formula_distance(Lat1, Long1, Lat2, Long2, Result_distanace),
    Airport12_Distance is Result_distanace.

%compute the time travelled by the plane
time_travel(Distance, Time_hrs) :-
    %plane travels in 500mi/hr constantly
    Time_hrs is Distance / 500.

%change time(hours, min) into min
time_to_hour(time(Hour, Minute), Hour_time) :-
    Hour_time is Hour + Minute / 60.

compute_final_flying_time(Airport1, Airport2, Time) :-
    airports_distance(Airport1, Airport2, Airport12_Distance),
    time_travel(Airport12_Distance, Traveltimes),
    Time is Traveltimes.

compute_landing_time(Start_time, Flying_time, Result_time) :-
    Result_time is Start_time + Flying_time.
 
%the end of recursion if we have arrived our destination
listpath(Final_destination, Final_destination, Current_time, Visited_airports, [Airport_list]).
%main recursive function to determine the travel Travel_routelist
listpath(Current_airport, Final_destination, Current_time, Visited_airports,  
    [Hour_time, Current_airport, Landing_time, Next_airport  | Airport_list]) :-

    %%get the flght information
    flight(Current_airport, Next_airport, Flying_time_vanilla),
    %% write('Current airport is: '), write(Current_airport), nl,
    %% write('Next airport is: '), write(Next_airport), nl,

    %Flying_time_vanilla has the form time(hrs, min)
    time_to_hour(Flying_time_vanilla, Hour_time),
    %% write('The flight time is: '), write(Hour_time), nl,

    %make sure we only check flights after the Hour_time
    Hour_time > Current_time,

    %check that we will not visit an airport that we have already visited
    not(member(Next_airport, Visited_airports)),
   
    %compute the final flying time
    compute_final_flying_time(Current_airport, Next_airport, Final_time ),
    %% write('Travel time is: '), write(Final_time), nl,
    compute_landing_time(Final_time, Hour_time, Landing),
    

    Time_now is Landing + 0.5,
    Landing_time is Landing, 
    %% write('Landing time is: '), write(Landing_time), nl,
    %% write('Free time is: '), write(Time_now), nl,
    %% write('Visited Airports: '),write(Visited_airports),nl,
    %% write('----------------------\n'),
    listpath(Next_airport, Final_destination, Time_now, [Next_airport | Visited_airports], Airport_list).
 
 %find the airport name from airport()   
find_airport_name(Airport, Name) :-
    airport(Airport, Name, _, _ ).
    %%write('find airport name ---is:'), write(Name), nl,
    %% Airport_name is Name,
    %% write('find airport name is:'), write(Name), nl.

%convert the airport code to upper case
%provided by the professor
to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

%get the hour time from the hour_decimal
get_hour_time(Hour_decimal, Hour_only) :-
    Hour_only is floor(Hour_decimal * 60) // 60.

%get the min time from the hour_decimal
get_minute_time(Hour_decimal, Minute_only) :-
    Minute_only is floor(Hour_decimal * 60) mod 60.

%print out the itinerary of the flight schedule 
writepath([Hour_time, Current_airport, Landing_time, Next_airport | Travel_route]) :-
    %% write('Current airport is:'), write(Current_airport), nl,
    %% write('Current airport is:'), write(Next_airport), nl,
    find_airport_name(Current_airport, Airport1_name),
    find_airport_name(Next_airport, Airport2_name),
    to_upper(Current_airport, Airport1_upper),
    to_upper(Next_airport, Airport2_upper),
    get_hour_time(Hour_time, Depart_hour),
    get_minute_time(Hour_time, Depart_minute),
    get_hour_time(Landing_time, Arrive_hour),
    get_minute_time(Landing_time, Arrival_minute),
    %% write('Current airport is:'), write(Airport1_name), nl,
    %% write('Next airport is:'), write(Airport2_name), nl,
    format('depart  %s  %s %02d:%02d', [Airport1_upper, Airport1_name, Depart_hour,Depart_minute]), nl,
    format('arrive  %s  %s %02d:%02d', [Airport2_upper, Airport2_name, Arrive_hour,Arrival_minute]), nl,
    %recursive call to itself
    writepath(Travel_route).

%the end of the recursion
%%the list will have some weird number at the end
writepath([Random | []]).


%error when starting and ending airports are the same
fly(Airport, Airport) :-
    write('Depature and Destination cannot be the same.'),
    !, fail.

%main call of this code, invoke the testing script
% ie. fly(Airport1, Airport2)
fly(Airport1, Airport2) :-
    %% write('Inside Main\n'),
    airport(Airport1, _, _, _),
    airport(Airport2, _, _, _),
    %% write('Departure is: '), write(Airport1), write('\n'),
    %% write('Arrival is: '), write(Airport2), write('\n'),
    airports_distance(Airport1, Airport2, Distance),
    time_travel(Distance, Time_hrs),
    %% write('Distance is: '), write(Distance), write('\n'),
    %% write('Time  is: '), write(Time_hrs), write('\n'),
    %% write('Entering the main function listpath\n'),

    %%arguments: departure, arrival, list of airports with time info, and 
    %% a list which keep tracks of the visited airports
    listpath(Airport1, Airport2, 0, [Airport1], Travel_route),
    %write(Travel_route),
    write('\n'),
    writepath(Travel_route).

%if there is no valid path from 1 to 2 
fly(Airport1, Airport2) :-
    airport(Airport1, _, _, _),
    airport(Airport2, _, _, _),
    to_upper(Airport1, Airport1_name),
    to_upper(Airport2, Airport2_name),
    format('Sorry, there is no valid itinerary between %s and %s.', 
        [Airport1_name, Airport2_name]),
    %terminate.
    !, fail.

%if invalid airport name
fly(Airport,Airport2) :-
    write('Sorry, airport name(s) does not exist in our database.'),
    !,fail.
