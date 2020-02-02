-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").


% Ship Details Display Code

fetch_ship_details([], Ship_ID) ->
  io:fwrite("No Ship present with the Ship ID = ~w~n", [Ship_ID]);

fetch_ship_details([_H | T], Ship_ID) ->
  if
    _H#ship.id == Ship_ID ->
      _H;
    true ->
      fetch_ship_details(T, Ship_ID)
  end.

get_ship(Shipping_State, Ship_ID) ->
  All_Ships = Shipping_State#shipping_state.ships,
  fetch_ship_details(All_Ships, Ship_ID).

% Container Details Display Code
fetch_container_details([], Container_ID) ->
  io:fwrite("No Container present with the Container_ID = ~w~n", [Container_ID]);

fetch_container_details([_H | T], Container_ID) ->
  if
    _H#container.id == Container_ID ->
      _H;
    true ->
      fetch_container_details(T, Container_ID)
  end.

get_container(Shipping_State, Container_ID) ->
  All_Containers = Shipping_State#shipping_state.containers,
  fetch_container_details(All_Containers, Container_ID).


% Port Details Display Code
fetch_port_details([], Port_ID) ->
  io:fwrite("No Port present with the Port_ID = ~w~n", [Port_ID]),
  error;

fetch_port_details([_H | T], Port_ID) ->
  if
    _H#port.id == Port_ID ->
      _H;
    true ->
      fetch_port_details(T, Port_ID)
  end.

get_port(Shipping_State, Port_ID) ->
  All_Ports = Shipping_State#shipping_state.ports,
  fetch_port_details(All_Ports, Port_ID).


% Occupied Port Docks Details Display Code
fetch_occupied_port_docks([], Port_ID, List_Docks) ->
  if
    length(List_Docks) == 0 ->
      io:fwrite("No Dock Occupied for Port Id ~n", [Port_ID]),
      error;
    true ->
      List_Docks
  end;

fetch_occupied_port_docks([_H | T], Port_ID, List_Docks) ->
  Location_Details = tuple_to_list(_H),
  Location_Port_Id = lists:nth(1, Location_Details),

  if
    Location_Port_Id == Port_ID ->
      New_List = lists:append(List_Docks, [lists:nth(2, Location_Details)]),
      fetch_occupied_port_docks(T, Port_ID, New_List);
    true ->
      fetch_occupied_port_docks(T, Port_ID, List_Docks)
  end.

get_occupied_docks(Shipping_State, Port_ID) ->
  Occupied_Locations = Shipping_State#shipping_state.ship_locations,
  fetch_occupied_port_docks(Occupied_Locations, Port_ID, []).

% Ship Location
fetch_ship_location([], Ship_ID) ->
  io:fwrite("The ship is not is not present ~n", [Ship_ID]),
  error
;

fetch_ship_location([_H | T], Ship_ID) ->
  Location_Details = tuple_to_list(_H),
  Location_Ship_Id = lists:nth(3, Location_Details),
  if
    Location_Ship_Id == Ship_ID ->
      {lists:nth(1, Location_Details), lists:nth(2, Location_Details)};
    true ->
      fetch_ship_location(T, Ship_ID)
  end.

get_ship_location(Shipping_State, Ship_ID) ->
  Occupied_Locations = Shipping_State#shipping_state.ship_locations,
  fetch_ship_location(Occupied_Locations, Ship_ID).

% Container Weights
add_Container_Weight([], ContainerId, Weight) ->
  Weight;
add_Container_Weight([_H | T], ContainerId, Weight) ->
  if
    _H#container.id == ContainerId ->
      New_Weight = Weight + _H#container.weight,
      add_Container_Weight([], ContainerId, New_Weight);
    true ->
      add_Container_Weight(T, ContainerId, Weight)
  end.

container_weight([], Containers, Weight) ->
  Weight;

container_weight([_H | T], Containers, Weight) ->
  New_Weight = add_Container_Weight(Containers, _H, Weight),
  container_weight(T, Containers, New_Weight).

get_container_weight(Shipping_State, Container_IDs) ->
  Containers = Shipping_State#shipping_state.containers,
  container_weight(Container_IDs, Containers, 0).

% Ship Weights
get_ship_weight(Shipping_State, Ship_ID) ->
  Ship_Inventory = Shipping_State#shipping_state.ship_inventory,
  Container_IDs = maps:get(Ship_ID, Ship_Inventory),
  get_container_weight(Shipping_State, Container_IDs).

% Load Weights onto Ship from Ports
fetch_port_ship([], Ship_ID, Port_Id) ->
  Port_Id;
fetch_port_ship([_H | T], Ship_ID, Port_Id) ->

  Ship_ID_Port = lists:nth(3, tuple_to_list(_H)),

  if
    Ship_ID == Ship_ID_Port ->
      fetch_port_ship([], Ship_ID, lists:nth(1, tuple_to_list(_H)));
    true ->
      fetch_port_ship(T, Ship_ID, Port_Id)
  end.

verify_containers_port(Container_IDs_Port, Container_IDs) ->

  Merged_list = lists:subtract(Container_IDs, Container_IDs_Port),

  if
    Merged_list == [] ->
      true;
    true ->
      false
  end.

fetch_ship_container_cap([], Capacity, Ship_Id) ->
  Capacity;
fetch_ship_container_cap([_H | T], Capacity, Ship_Id) ->
  if
    _H#ship.id == Ship_Id ->

      fetch_ship_container_cap([], _H#ship.container_cap, Ship_Id);
    true ->
      fetch_ship_container_cap(T, Capacity, Ship_Id)
  end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->

  Locations = Shipping_State#shipping_state.ship_locations,
  Ship_port_id = fetch_port_ship(Locations, Ship_ID, -1),

  if
    -1 /= Ship_port_id ->
      Port_Inventory = Shipping_State#shipping_state.port_inventory,
      Container_IDs_Port = maps:get(Ship_port_id, Port_Inventory),
      Flag = verify_containers_port(Container_IDs_Port, Container_IDs),
      if
        Flag ->
          Ship_Inventory_Map = Shipping_State#shipping_state.ship_inventory,
          Ship_Inventory = maps:get(Ship_ID, Ship_Inventory_Map),

          Ship_Max_Capacity = fetch_ship_container_cap(Shipping_State#shipping_state.ships, -1, Ship_ID),

          if
            Ship_Max_Capacity /= -1 ->
              Length_of_Existing_Container = length(Ship_Inventory),
              Length_of_Additional_Container = length(Container_IDs),

              if
                Length_of_Existing_Container + Length_of_Additional_Container =< Ship_Max_Capacity ->
                  New_Ship_Inventory = lists:merge(Ship_Inventory, Container_IDs),
                  New_Ship_Inventory_Map = maps:put(Ship_ID, New_Ship_Inventory, Ship_Inventory_Map),

                  New_Container_IDs_Port = Container_IDs_Port -- Container_IDs,
                  New_Port_Inventory = maps:put(Ship_port_id, New_Container_IDs_Port, Port_Inventory),

                  New_Shipping_State = Shipping_State#shipping_state{port_inventory = New_Port_Inventory},
                  Final_Shipping_State = New_Shipping_State#shipping_state{ship_inventory = New_Ship_Inventory_Map},

                  {ok, Final_Shipping_State};
                true ->
                  % containers cannot be loaded to ship as loading will surpass max capacity of ship
                  error
              end
          ;
            true ->
              % Ship max capacity not given in data
              error
          end
      ;
        true ->
          % All containers not at same port
          error
      end
  ;
    true ->
      % Ship not present at any port
      error
  end.

% Unload whole ship to the port

fetch_port_container_cap([], Capacity, Port_Id) ->
  Capacity;
fetch_port_container_cap([_H | T], Capacity, Port_Id) ->
  if
    _H#port.id == Port_Id ->
      fetch_port_container_cap([], _H#port.container_cap, Port_Id);

    true ->
      fetch_port_container_cap(T, Capacity, Port_Id)
  end.

unload_ship_all(Shipping_State, Ship_ID) ->

  Locations = Shipping_State#shipping_state.ship_locations,
  Ship_port_id = fetch_port_ship(Locations, Ship_ID, -1),
  if
    Ship_port_id /= -1 ->

      Port_Inventory = Shipping_State#shipping_state.port_inventory,
      Container_IDs_Port = maps:get(Ship_port_id, Port_Inventory),

      Ship_Inventory_Map = Shipping_State#shipping_state.ship_inventory,
      Ship_Inventory = maps:get(Ship_ID, Ship_Inventory_Map),
      Port_Max_Capacity = fetch_port_container_cap(Shipping_State#shipping_state.ports, -1, Ship_port_id),
      if
        Port_Max_Capacity /= -1 ->
          if
            length(Ship_Inventory) + length(Container_IDs_Port) =< Port_Max_Capacity ->
              New_Container_IDs_Port = lists:merge(Container_IDs_Port, Ship_Inventory),
              New_Ship_Inventory_Map = maps:put(Ship_ID, [], Ship_Inventory_Map),
              New_Port_Inventory = maps:put(Ship_port_id, New_Container_IDs_Port, Port_Inventory),

              New_Shipping_State = Shipping_State#shipping_state{ship_inventory = New_Ship_Inventory_Map},
              Final_Shipping_State = New_Shipping_State#shipping_state{port_inventory = New_Port_Inventory},
              {ok, Final_Shipping_State};
            true ->
              % cannot unload as port capacity is surpassed
              error
          end;
        true ->
          % Port capacity is not specified
          error
      end;
    true ->
      % Ship not present at any port
      error
  end.

% Unload Ship from Container Id
verify_ship_containers(Ship_ContainerId, Container_IDs) ->
  Difference_List = lists:subtract(Container_IDs, Ship_ContainerId),
  if
    Difference_List == [] ->
      true;
    true ->
      false
  end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->

  Ships_Inventory = Shipping_State#shipping_state.ship_inventory,
  Ship_Inventory = maps:get(Ship_ID, Ships_Inventory),
  Verification_Flag = verify_ship_containers(Ship_Inventory, Container_IDs),

  if
    Verification_Flag ->
      Locations = Shipping_State#shipping_state.ship_locations,
      Ship_port_id = fetch_port_ship(Locations, Ship_ID, -1),
      if
        Ship_port_id /= -1 ->
          Port_Inventory = Shipping_State#shipping_state.port_inventory,
          Container_IDs_Port = maps:get(Ship_port_id, Port_Inventory),

          Port_Max_Capacity = fetch_port_container_cap(Shipping_State#shipping_state.ports, -1, Ship_port_id),
          if
            Port_Max_Capacity /= -1 ->
              if
                length(Container_IDs) + length(Container_IDs_Port) =< Port_Max_Capacity ->
                  New_Container_IDs_Port = lists:merge(Container_IDs_Port, Container_IDs),
                  New_Ship_Inventory = Ship_Inventory -- Container_IDs,

                  New_Ship_Inventory_Map = maps:put(Ship_ID, New_Ship_Inventory, Ships_Inventory),
                  New_Port_Inventory = maps:put(Ship_port_id, New_Container_IDs_Port, Port_Inventory),

                  New_Shipping_State = Shipping_State#shipping_state{ship_inventory = New_Ship_Inventory_Map},
                  Final_Shipping_State = New_Shipping_State#shipping_state{port_inventory = New_Port_Inventory},
                  {ok, Final_Shipping_State}
              ;
                true ->
                  % Port Capacity Surpassed
                  error
              end
          ;
            true ->
              % Not able to fetch port max capacity
              error
          end
      ;
        true ->
          % Ship is not dock to any port
          error
      end;

    true ->
      % Containers are not present in the list
      error
  end.


% Set Sail

check_dock_occupied([], {Port_ID, Dock}) ->
  false;
check_dock_occupied([_H | T], {Port_ID, Dock}) ->

  Location_Dtls = tuple_to_list(_H),
  Current_Port_Id = lists:nth(1, Location_Dtls),
  Current_Dock = lists:nth(2, Location_Dtls),
  if
    is_atom(Dock) ->
      EqualFlag = string:equal(atom_to_list(Dock), atom_to_list(Current_Dock));
    true ->
      EqualFlag = string:equal(Dock, atom_to_list(Current_Dock))
  end,

  if
    (Port_ID == Current_Port_Id) and EqualFlag ->
      true;
    true ->
      check_dock_occupied(T, {Port_ID, Dock})
  end.


fetch_ship_dock([], ShipId) ->
  -1;
fetch_ship_dock([_H | T], ShipId) ->
  Location_Dtls = tuple_to_list(_H),
  Current_Ship_Id = lists:nth(3, Location_Dtls),

  if
    Current_Ship_Id == ShipId ->
      _H;
    true ->
      fetch_ship_dock(T, ShipId)
  end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
  Locations = Shipping_State#shipping_state.ship_locations,
  Occupancy = check_dock_occupied(Locations, {Port_ID, Dock}),
  if
    Occupancy ->
      % Port already occupied
      error;
    true ->

      Element_Delete = fetch_ship_dock(Locations, Ship_ID),
      if
        is_atom(Dock) ->
          New_Ship_Location = {Port_ID, Dock, Ship_ID};
        true ->
          New_Ship_Location = {Port_ID, list_to_atom(Dock), Ship_ID}
      end,

      New_Locations = lists:delete(Element_Delete, Locations),
      Final_Locations = lists:append([New_Ship_Location], New_Locations),
      Final_Shipping_State = Shipping_State#shipping_state{ship_locations = Final_Locations},
      {ok,Final_Shipping_State}
  end.




%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
  lists:all(fun(Elem) -> lists:member(Elem, Target_List) end, Sub_List).


%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
  io:format("--Ships--~n"),
  _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
  io:format("--Ports--~n"),
  _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([Port = #port{id = Port_ID} | _], Port_ID) -> Port;
get_port_helper([_ | Other_Ports], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
  case Ships of
    [] ->
      ok;
    [Ship | Other_Ships] ->
      {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
      Port = get_port_helper(Ports, Port_ID),
      {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
      io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
      print_ships(Other_Ships, Locations, Inventory, Ports)
  end.

print_containers(Containers) ->
  io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
  case Ports of
    [] ->
      ok;
    [Port | Other_Ports] ->
      {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
      io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
      print_ports(Other_Ports, Inventory)
  end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
  Ships = [#ship{id = 1, name = "Santa Maria", container_cap = 20},
    #ship{id = 2, name = "Nina", container_cap = 20},
    #ship{id = 3, name = "Pinta", container_cap = 20},
    #ship{id = 4, name = "SS Minnow", container_cap = 20},
    #ship{id = 5, name = "Sir Leaks-A-Lot", container_cap = 20}
  ],
  Containers = [
    #container{id = 1, weight = 200},
    #container{id = 2, weight = 215},
    #container{id = 3, weight = 131},
    #container{id = 4, weight = 62},
    #container{id = 5, weight = 112},
    #container{id = 6, weight = 217},
    #container{id = 7, weight = 61},
    #container{id = 8, weight = 99},
    #container{id = 9, weight = 82},
    #container{id = 10, weight = 185},
    #container{id = 11, weight = 282},
    #container{id = 12, weight = 312},
    #container{id = 13, weight = 283},
    #container{id = 14, weight = 331},
    #container{id = 15, weight = 136},
    #container{id = 16, weight = 200},
    #container{id = 17, weight = 215},
    #container{id = 18, weight = 131},
    #container{id = 19, weight = 62},
    #container{id = 20, weight = 112},
    #container{id = 21, weight = 217},
    #container{id = 22, weight = 61},
    #container{id = 23, weight = 99},
    #container{id = 24, weight = 82},
    #container{id = 25, weight = 185},
    #container{id = 26, weight = 282},
    #container{id = 27, weight = 312},
    #container{id = 28, weight = 283},
    #container{id = 29, weight = 331},
    #container{id = 30, weight = 136}
  ],
  Ports = [
    #port{
      id = 1,
      name = "New York",
      docks = ['A', 'B', 'C', 'D'],
      container_cap = 200
    },
    #port{
      id = 2,
      name = "San Francisco",
      docks = ['A', 'B', 'C', 'D'],
      container_cap = 200
    },
    #port{
      id = 3,
      name = "Miami",
      docks = ['A', 'B', 'C', 'D'],
      container_cap = 200
    }
  ],
  %% {port, dock, ship}
  Locations = [
    {1, 'B', 1},
    {1, 'A', 3},
    {3, 'C', 2},
    {2, 'D', 4},
    {2, 'B', 5}
  ],
  Ship_Inventory = #{
    1=>[14, 15, 9, 2, 6],
    2=>[1, 3, 4, 13],
    3=>[],
    4=>[2, 8, 11, 7],
    5=>[5, 10, 12]},
  Port_Inventory = #{
    1=>[16, 17, 18, 19, 20],
    2=>[21, 22, 23, 24, 25],
    3=>[26, 27, 28, 29, 30]
  },
  ShippingState = #shipping_state{ships = Ships, containers = Containers, ports = Ports,
    ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory},
  ShippingState.