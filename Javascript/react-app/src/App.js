import { Route, Switch } from "react-router-dom";
import Layout from "./components/layout/Layout";
// Route defines the path that renders a page component. This is path after the domain.
// Switch makes the page components swichable when you move to a different path. This avoids rendering the home and subsequent pages
import AllMeetupsPage from "./pages/AllMeetups";
import FavoritesPage from "./pages/Favorites";
import NewMeetupPage from "./pages/NewMeetup";

function App() {
  return (
    <div>
      <Layout>
        <Switch>
          <Route path="/" exact>
            {/*exact makes sure that the page component is rendered when the path is exactly / and not anything that contains / */}
            <AllMeetupsPage />
          </Route>
          <Route path="/new-meetup">
            <NewMeetupPage />
          </Route>
          <Route path="/favorites">
            <FavoritesPage />
          </Route>
        </Switch>
      </Layout>
    </div>
  );
}

export default App;
