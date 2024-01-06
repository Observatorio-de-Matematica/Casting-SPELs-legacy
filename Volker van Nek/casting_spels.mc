objects: '[whiskey_bottle, bucket, chain, frog]$


map: '[[living_room,
         "you are in the living_room of a wizards house. there is a wizard snoring loudly on the couch. ",
         [west, door, garden],
         [upstairs, stairway, attic] ],
         
       [garden, 
         "you are in a beautiful garden. there is a well in front of you. ",
         [east, door, living_room] ],
         
       [attic,
         "you are in the attic of the wizards house. there is a giant welding torch in the corner. ",
         [downstairs, stairway, living_room] ]]$

object_locations: '[[whiskey_bottle, living_room],
                    [bucket, living_room],
                    [chain, garden],
                    [frog, garden]]$

location: 'living_room$


assoc_(key,alist):= block(
   [ result:false ],
   for elem in alist do 
      if key=first(elem) then return(result:elem),
   result )$


describe_location(location,map):= 
   second( assoc_(location,map) )$


describe_path(path):=
   sconcat("there is a ", path[2], " going ", path[1], " from here. ")$


describe_paths(location,map):=
   apply( sconcat, map( describe_path,rest( assoc_(location,map),2 ) ) )$


is_at(obj,loc,obj_loc):= block(
   [ temp:assoc_(obj,obj_loc) ],
   listp(temp) and is(second(temp)=loc) )$   


describe_floor(loc,objs,obj_loc):=
   apply( sconcat,  
          map( lambda([x],sconcat("you see a ",x," on the floor. ")),
               sublist( objs, lambda([x],is_at(x,loc,obj_loc)) ) ))$


l_o_o_k():=
   sconcat( describe_location(location,map),
            describe_paths(location,map),
            describe_floor(location,objects,object_locations) )$
nofix("look")$
"look"():= l_o_o_k()$


prefix("walk")$
"walk"(direction):= w_a_l_k__direction(direction)$

w_a_l_k__direction(direction):= block(
  [ next_: assoc_( direction,rest( assoc_(location,map), 2 ) ) ],
  if listp(next_) then (
     location: third (next_),
     l_o_o_k() )
  else "you cant go that way. ")$
  

prefix("pickup")$
"pickup"(object):= 
   if is_at( object,location,object_locations ) then (
      object_locations: cons([object,'body],object_locations),
      sconcat("you are now carrying the ",object,".") )
   else "you cannot get that. "$


at_body():=
   sublist( objects,
            lambda([x],is_at(x,'body,object_locations)) )$


nofix("inventory")$
"inventory"():= 
   apply( sconcat,
      map( lambda([x],sconcat(x," ")),
           at_body() ))$

have(object):=
   member( object,at_body() )$


SPEL([rest])::= buildq(
   [rest],
   buildq(splice(rest)) )$

game_action(command,subj,obj,place,[rest])::= SPEL( 
   [command,subj,obj,place,rest], 
   block( 
      infix(command), 
      command(subject,object):= block(
         if location = place 
            and subject = subj
            and object = obj 
            and have(subj) then apply(sconcat,rest)
         else sconcat("you cannot ",command," like that. ") )))$

chain_welded: false$

game_action("weld",chain,bucket,attic,
   if have(bucket)      
   and not chain_welded then (
      chain_welded: true,
      "the chain is now securely welded to the bucket. " )
   else "you do not have a bucket. ")$

bucket_filled: false$

game_action("dunk",bucket,well,garden,
   if chain_welded then (
      bucket_filled: true,
      "the bucket is now full of water. " )
   else "the water level is too low to reach. ")$

game_action("splash",bucket,wizard,living_room,
   if not bucket_filled then "the bucket has nothing in it. "
   else if have(frog) then sconcat(
      "the wizard awakens and sees that you stole his frog. ",
      "he is so upset he banishes you to the netherworlds ",
      "- you lose! the end. ")
   else sconcat(
      "the wizard awakens from his slumber and greets you warmly. ",
      "he hands you the magic low_carb donut - you win! the end. ") )$
   

set_object_locations():= block(
   set_random_state( make_random_state(true) ),
   location: first( map[random(3)+1] ),
   for n from 1 thru 3 do 
      object_locations[n][2]: first( map[random(3)+1] ),
   'done )$
   
prefix("casting")$

"casting"(SPELs):= block(
   chain_welded: false,
   bucket_filled: false,
   object_locations: 
      sublist( object_locations, lambda([x],not second(x)='body) ),
   set_object_locations(),
   "have a lot of fun. " )$
