module Types

type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
    | BlockQuote of list<MarkdownBlock>
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>

and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan = 
    | Literal of string
    | InlineCode of string
    | Strong of MarkdownSpans
    | Emphasis of MarkdownSpans
    | Hyperlink of MarkdownSpans * string
    | HardLineBreak
