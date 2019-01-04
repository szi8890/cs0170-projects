#use "eliza_support.ml" ;;

let my_rules =
  [Rule([Any; Lit("bought"); Any],
       [Text(["How"; "much"; "did"]); Place(2); Text(["cost?"])]);

  Rule([Lit("My"); One; Lit("has"); Any],
       [Text(["Is"; "it"; "good"; "for"]); Place(1);
       Text(["to"; "have"]); Place(2); Text(["?"])]);

  Rule([One; Lit("is"); Lit("my"); Lit("best"); Lit("friend")],
       [Text(["Tell"; "me"; "more"; "about"]); Place(1)]);

  Rule([Lit("Do"); Lit("you"); Lit("like"); Any; One; Lit("or"); Any; One],
      [Text(["I"; "like"]); Place(3); Place(4); Text(["more"; "than"]);
      Place(1); Place(2)]);

  Rule([Lit("I"); One; Any; Lit("you")],
        [Text(["Why"; "do"; "you"]); Place(1); Place(2); Text(["me?"])]);

  Rule([Any; Lit("is"); One; Lit("years"); Lit("old")],
       [Text(["WOW"; "that's"; "old"])]);

  Rule([Any; Lit("?")], [Text(["No"; "questions,"; "only"; "answers"])]);

  Rule([Lit("I"); Lit("enjoy"); Lit("sleeping"); Any; Any],
      [Text(["Do"; "you"; "really"; "enjoy"; "sleeping"]);
      Place(1); Place(2)]);

  Rule([Lit("I"); Any; Lit("everyday")],
       [Text(["Do"; "you"; "like"; "to"]); Place(1); Text(["everyday?"])]);

  Rule([Lit("Are"); Lit("you"); Lit("a"); Lit("bot")],
      [Text(["Not"; "a"; "robot."])]);

  Rule([One; One; One],
      [Text(["Can"; "you"; "repeat"; "that?"])]);

  Rule([Any],
       [Text(["Howdy" ; "partner,"; "how" ; "are" ; "you?"])])] ;;
