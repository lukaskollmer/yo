"use strict";

const BaseCompiler = require('../base-compiler'),
    path = require("path");

class YoCompiler extends BaseCompiler {
    constructor(info, env) {
        info.supportsBinary = false;
        info.supportsExecute = false;
        info.supportsDemangle = false;
        super(info, env);

        this.compiler.supportsIntel = true;
        this.compiler.supportsIrView = true;
        this.compiler.supportsDemangle = false;
        this.demanglerClass = undefined;
        this.compiler.irArg = ['--emit', 'llvm-ir'];
    }

    getDefaultFilters() {
        let filters = super.getDefaultFilters();
        filters.demangle = false;
        return filters;
    }

    optionsForFilter(filters, outputFilename) {
        let opts = ['-g', '-emit', 'asm'];
        // TODO add outputFilename once the -o flag is implemented

        if (filters.intel && !filters.binary) {
            opts.push('--x86-asm-syntax=intel');
        }
        return opts;
    }

    getOutputFilename(dirPath) {
        return path.join(dirPath, `${this.compileFilename}.s`);
    }

    // Override the IR file name method for rustc because the output file is different from clang.
    getIrOutputFilename(inputFilename) {
        return inputFilename + '.ll';
    }

    exec(compiler, args, options) {
        options.env['YO_DISABLE_EXEC'] = '';
        return super.exec(compiler, args, options);
    }
}

module.exports = YoCompiler;
