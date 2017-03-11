@Rendering

Feature: Rendering MdParagraphs into formatted strings

  Scenario: Rendering code
    Given an MdDocument exists containing only MdCode
    When I render the document
    Then I receive an attributed string
    And the result is properly formatted code

  Scenario: rendering a string containing a plain link
    Given an MdDocument exists containing a string with an unreferenced link
    When I render the document
    Then I receive an attributed string
    And the unreferenced link has been properly formatted

  Scenario: rendering a string containing a referenced link
    Given an MdDocument exists containing a string with a referenced link
    When I render the document
    Then I receive an attributed string
    And the referenced link has been properly formatted

  Scenario: rendering a string containing a labeled link
    Given an MdDocument exists containing a string with a labeled link
    When I render the document
    Then I receive an attributed string
    And the labeled link has been properly formatted

  Scenario: rendering a string containing a referenced link without an alternative name
    Given an MdDocument exists containing a string with a monadic referenced link
    When I render the document
    Then I receive an attributed string
    And the referenced link has been properly formatted

    @wip
  Scenario: rendering a string with a mixture of link styles
    Given an MdDocument exists containing a string with a mixture of link styles
    When I render the document
    Then I receive an attributed string
    And the result has been properly formatted
