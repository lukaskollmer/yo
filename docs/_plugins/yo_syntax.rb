module Jekyll
  class YoSyntax < Converter
    $yo_bin = "../Debug/yo"
    safe true
    priority :high

    def matches(ext)
      ext =~ /^\.md$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      content
      # content
      #   .sub("$_YO_CLI_HELP_$", `#{$yo_bin} -help`)
      #   .gsub(/```yo\n(.*?)\n```/m) do |match|
      #     "```yo\n#{$1}\n```"
      #   end
    end
  end
end