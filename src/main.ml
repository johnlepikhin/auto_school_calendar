
open Config
			 
let timezone_gap = -. (3600.*.3.)
			 
let redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
let scope = [GapiCalendarV3Service.Scope.calendar]

open GapiCalendarV3Model
open GapiCalendarV3Service
		      
let configuration =
  GapiConfig.(
  { default with
    application_name = my_application_name;
    auth = OAuth2 {
	       client_id = my_client_id;
	       client_secret = my_client_secret;
	       refresh_access_token = None;
	     };
  })

(*
let authorization_url =
  GapiOAuth2.authorization_code_url
    ~redirect_uri
    ~scope
    ~response_type:"code"
    my_client_id

let () = print_endline "Go to the following link in your browser:";
         print_endline authorization_url

let () = print_endline "What is the authorization code?"
let code = input_line stdin
 *)

let get_events session calendarId =
  let rec get_events next_page_token session lst =
    let (events, session') =
      EventsResource.list
        ~pageToken:next_page_token
        ~calendarId
        session
    in

    let lst = lst @ events.Events.items in
    if events.Events.nextPageToken <> "" then
      get_events events.Events.nextPageToken session' lst
    else
      session', lst
  in
  let (session, lst) = get_events "" session [] in
  (session, lst)
	    
let delete_events session calendarId events =
  List.fold_left
    (fun session' event ->
     let ((), session') =
       EventsResource.delete
	 ~calendarId
	 ~eventId:event.Event.id
	 session
     in
     session'
    )
    session events

let calendar_cleanup session calendarId =
  let (session, events) = get_events session calendarId in
  delete_events session calendarId events

let create_event session name _start _end =
  let event = {
      Event.empty with
      Event.summary = name;
      Event.start = { EventDateTime.empty with
                      EventDateTime.dateTime =
                        Netdate.create _start
		    };
      Event._end = { EventDateTime.empty with
                     EventDateTime.dateTime =
                       Netdate.create _end
		   };
    }
  in
  let (created_event, session) =
    EventsResource.insert
      ~calendarId:Class5k.google_calendar_id
      event
      session
  in
  (created_event, session)

let () =
  GapiConversation.with_curl
    configuration
    (fun session ->

     (*
     (* Step 2: Exchange --> *)
     let (response, session) =
       GapiOAuth2.get_access_token
         ~client_id:my_client_id
         ~client_secret:my_client_secret
         ~code
         ~redirect_uri
         session in
     let (access_token, refresh_token) =
       match response with
         GapiAuthResponse.OAuth2AccessToken token ->
         (token.GapiAuthResponse.OAuth2.access_token,
          token.GapiAuthResponse.OAuth2.refresh_token)
       | _ -> failwith "Not supported OAuth2 response" in
     (* End of Step 2 <-- *)
     
     Printf.printf "%s   ---    %s\n" access_token refresh_token
      *)

     let session = {
         session with
         GapiConversation.Session.auth =
           GapiConversation.Session.OAuth2 {
               GapiConversation.Session.oauth2_token = access_token;
               refresh_token
	     }
       }
     in

     (* Retrieving calendars in a user's calendar list *)
     let session =
       let rec get_calendar_list next_page_token session =
         let (calendar_list, session') = CalendarListResource.list session in

         List.iter
           (fun calendar ->
            Printf.printf "%s :: %s\n" calendar.CalendarListEntry.summary calendar.CalendarListEntry.id)
           calendar_list.CalendarList.items;

         if calendar_list.CalendarList.nextPageToken <> "" then
           get_calendar_list
             calendar_list.CalendarList.nextPageToken session'
         else
           session'
       in
       get_calendar_list "" session
     in


     let session = calendar_cleanup session Class5k.google_calendar_id in

     let session = Lessons.list (module Class5k) Lessons.year_session
     |> List.fold_left
	  (fun session (i, l) ->
	   let open CalendarLib.Calendar in
	   let _start = i.MyCalendar.Interval._start |> to_unixfloat |> (+.) timezone_gap in
	   let _end = i.MyCalendar.Interval._end |> to_unixfloat |> (+.) timezone_gap in
	   let i = MyCalendar.Interval.to_string i in
	   Printf.printf "%s : %s\n" i l; flush_all ();
	   let (created_event, session) = create_event session l _start _end in
	   session
	  ) session
     in
     ()

    )
