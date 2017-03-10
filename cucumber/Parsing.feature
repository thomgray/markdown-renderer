@Parsing

Feature: Parsing raw string to markdown

  Scenario: parsing a nested check list
    When I parse a string containing a nested check list
    Then I receive a document containing 1 paragraph
    And the 1st paragraph is a task list containing a single item with a single nested item

  Scenario: parsing a string containing a link
    When I parse a string containing an "unreferenced" link
    Then I receive a document containing 1 paragraph
    And the 1st paragraph is a "string"
    And the 1st paragraph contains an "unreferenced" link

  Scenario: parsing a string containing a link
    When I parse a string containing a "labeled" link
    Then I receive a document containing 1 paragraph
    And the 1st paragraph is a "string"
    And the 1st paragraph contains a "labeled" link

  Scenario: parsing a string containing a link
    When I parse a string containing an "referenced" link
    Then I receive a document containing 1 paragraph
    And the 1st paragraph is a "string"
    And the 1st paragraph contains an "referenced" link