import NewMeetupForm from "../components/meetups/NewMeetupForm";
import { useHistory } from "react-router-dom"; //navigates the app programatically

function NewMeetupPage() {
  const history = useHistory();
  function addMeetupHandler(meetupData) {
    fetch(
      "https://react-getting-started-c1293-default-rtdb.firebaseio.com/meetups.json", //create a meetups node in which we are going to have our submitted data.
      {
        method: "POST", //request type POST sends data to API
        body: JSON.stringify(meetupData), //converts JS values to JSON
        headers: {
          "Content-Type": "application/json", //metadata indicating this request sends JSON data.
        }, //sending a request to firebase API which then stores the data in the data.
      }
    ).then(() => {
      history.replace("/"); //allows us to navigate to / after data has been submitted without an option for going back to the form
    });
  }
  return (
    <section>
      <h1> Add New Meetup Page</h1>
      <NewMeetupForm onAddMeetup={addMeetupHandler} />
    </section>
  );
}

export default NewMeetupPage;
