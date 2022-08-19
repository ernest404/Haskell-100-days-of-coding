import { useContext } from "react";
import FavoritesContext from "../store/favorite-context";
import MeetupList from "../components/meetups/MeetupList";

function FavoritesPage(params) {
  const favoritesCtx = useContext(FavoritesContext);

  let content;
  if (favoritesCtx.totalFavorites === 0) {
    content = <p>No favourites</p>;
  } else {
    content = <MeetupList meetups={favoritesCtx.favouritesCtx} />;
  }
  return (
    <section>
      <h1>My favorites</h1>
      <MeetupList meetups={content} />
    </section>
  );
}

export default FavoritesPage;
