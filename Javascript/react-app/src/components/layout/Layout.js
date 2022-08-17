import classes from "./Layout.module.css";
import MainNavigation from "./MainNavigation";

function Layout(props) {
  return (
    <div>
      <MainNavigation />
      <main className={classes.main}>{props.children}</main>
      {/* prop.children allows sandwiched values to be displayed as childern of a tag
      classes.main labels a tag with a class name to allow for styling with css module */}
    </div>
  );
}

export default Layout;
