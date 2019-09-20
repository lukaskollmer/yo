//
//  Pygmentize.cpp
//  yo
//
//  Created by Lukas Kollmer on 2019-07-23.
//  Copyright © 2019 Lukas Kollmer. All rights reserved.
//

#include "Pygmentize.h"

#include <sstream>



void repr_token(std::ostringstream &OS, const yo::parser::Token &token);



std::string yo::lex::pygmentize(const std::vector<parser::Token> &tokens) {
    std::ostringstream html;
    html << "<div class=\"language-yo\">";
    html << "<div class=\"highlight\">";
    html << "<pre class=\"highlight\">";
    html << "<code>";
    
    for (const auto& token : tokens) {
        repr_token(html, token);
    }
    
    html << "</code>";
    html << "</pre>";
    html << "</div>";
    html << "</div>";
    
    
    return html.str();
}




static const std::string DBL_QT = "\"";



using TK = yo::parser::Token::TokenKind;


static std::map<TK, std::string_view> tokenKindClassMappings = {
    { TK::Ident, "" }
};

// Classname mapping: see comments in https://github.com/richleland/pygments-css/blob/master/default.css

void repr_token(std::ostringstream &OS, const yo::parser::Token &token) {
    switch (token.getKind()) {
        case TK::EOF_:
            break;
        case TK::Whitespace:
            OS << token.getSourceText();
            break;
        default:
            OS << "<span class=\"" << tokenKindClassMappings[token.getKind()] << "\">" << token.getSourceText() << "</span>";
            break;
    }
    
}



