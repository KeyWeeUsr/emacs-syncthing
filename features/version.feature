Feature: Connect to already online Syncthing server
  Scenario Outline: Can connect after token input
    Given Server "<version>" is running in the background
    And I am running client in "<mode>" mode

    When I have no API token set
    And I launch client "<first-launch>"

    Then client redirects to token customization

    When I set a API token in "<how>" to "<token>"
    And I launch client "<second-launch>"
    Then client buffer header contains "<header contains>"

    Examples:
      | version | mode            | first-launch | token    | how         | second-launch | header contains |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.26.0 | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.26.0 | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.26.0 | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.26.0 | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <customize> | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <customize> | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <manual>    | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <manual>    | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.29.6 | non-interactive | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.29.6 | non-interactive | <fail>       | <apikey> | <setq>      | <success>     | <version>       |
      |  1.29.6 | interactive     | <fail>       | <empty>  | <setq>      | <fail>        | <empty>         |
      |  1.29.6 | interactive     | <fail>       | <apikey> | <setq>      | <success>     | <version>       |

  Scenario Outline: Can connect with a pre-configured key
    Given Server "<version>" is running in the background
    And I am running client in "<mode>" mode

    When I set a API token in "<how>" to "<token>"
    And I launch client "<launch>"
    Then client buffer header contains "<header contains>"

    Examples:
      | version | mode            | token    | how      | launch    | header contains |
      |  1.26.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.26.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |  1.26.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.26.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |  1.26.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.26.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |  1.26.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.26.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |  1.26.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.26.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |  1.26.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.26.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |  1.26.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.26.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |  1.26.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.26.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |  1.29.6 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.29.6 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |  1.29.6 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.29.6 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |  1.29.6 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.29.6 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |  1.29.6 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |  1.29.6 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |  1.29.6 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.29.6 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |  1.29.6 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.29.6 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |  1.29.6 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.29.6 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |  1.29.6 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |  1.29.6 | interactive     | <apikey> | <setq>   | <success> | <version>       |
