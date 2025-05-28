Feature: Connect to already online Syncthing server
  Scenario Outline: Can connect after token input
    Given Server is running in the background
    And I am running client in "<mode>" mode

    When I have no API token set
    And I launch client "<first-launch>"

    Then client redirects to token customization

    When I set a API token in "<how>" to "<token>"
    And I launch client "<second-launch>"
    Then client buffer header contains "<header contains>"

    Examples:
      | mode            | first-launch | token    | how         | second-launch | header contains |
      | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |

  Scenario Outline: Can connect with a pre-configured key
    Given Server is running in the background
    And I am running client in "<mode>" mode

    When I set a API token in "<how>" to "<token>"
    And I launch client "<launch>"
    Then client buffer header contains "<header contains>"

    Examples:
      | mode            | token    | how         | launch    | header contains |
      | non-interactive | <empty>  | <manual>    | <fail>    | <empty>         |
      | non-interactive | <apikey> | <manual>    | <success> | <version>       |
      | interactive     | <empty>  | <manual>    | <fail>    | <empty>         |
      | interactive     | <apikey> | <manual>    | <success> | <version>       |
      | non-interactive | <empty>  | <manual>    | <fail>    | <empty>         |
      | non-interactive | <apikey> | <manual>    | <success> | <version>       |
      | interactive     | <empty>  | <manual>    | <fail>    | <empty>         |
      | interactive     | <apikey> | <manual>    | <success> | <version>       |
      | non-interactive | <empty>  | <setq>      | <fail>    | <empty>         |
      | non-interactive | <apikey> | <setq>      | <success> | <version>       |
      | interactive     | <empty>  | <setq>      | <fail>    | <empty>         |
      | interactive     | <apikey> | <setq>      | <success> | <version>       |
      | non-interactive | <empty>  | <setq>      | <fail>    | <empty>         |
      | non-interactive | <apikey> | <setq>      | <success> | <version>       |
      | interactive     | <empty>  | <setq>      | <fail>    | <empty>         |
      | interactive     | <apikey> | <setq>      | <success> | <version>       |
