#!/usr/bin/env python3

import subprocess, os, argparse
from pyfzf.pyfzf import FzfPrompt # pyfzf
fzf = FzfPrompt()

def parse_arguments():
    parser = argparse.ArgumentParser(description='Script to process input file')
    parser.add_argument('-i', '--input', required=True, help='Input file path')
    parser.add_argument('output', nargs='+', help='Output file path')

    args = parser.parse_args()
    return args

def get_yes_no_input(prompt, default=None):
    """Prompt user for yes/no input with optional default option."""
    while True:
        if default is None:
            choice = input(prompt + " (y/n): ").strip().lower()
        elif default.lower() in {'y', 'yes'}:
            choice = input(prompt + " (Y/n): ").strip().lower()
        elif default.lower() in {'n', 'no'}:
            choice = input(prompt + " (y/N): ").strip().lower()
        else:
            raise ValueError("Default option must be 'y', 'yes', 'n', or 'no' (case insensitive)")

        if choice in {'y', 'yes'}:
            return True
        elif choice in {'n', 'no'}:
            return False
        elif choice == '' and default is not None:
            return default.lower() in ('y', 'yes')  # Return default if no input and default is provided
        else:
            print("Please respond with 'y' or 'n'.")

def get_fzf_input(options: list[str], args: str = ""):
    selected_option = fzf.prompt(options, args)
    # Print the selected option
    print(selected_option)

def ffmpeg_run(inputpath, outputpath, args):
    # "ffmpeg -i input.m4a -vn -c:a libvorbis -q:a 8 output.ogg"
    ffmpeg_cmd = f"ffmpeg -i {inputpath} {args} {outputpath}"
    result = subprocess.run(ffmpeg_cmd, shell=True, capture_output=True, text=True)
    print(f"> ffmpeg result: {result.stdout}")

def main():
    ## Todo:
    # use argparse or click? will very help
    ## args:
    ##
    # move prompts and fzf functions into separate binary?

    ## Steps:
    # process args: inputpath and outputpath, path error checking, ffmpeg args
    # confirm inputoutput, ffmpeg command to run

    # process args
    args = parse_arguments()
    input_file_path = args.input
    output_file_path = args.output
    cmd = "ffmpeg -i " + input_file_path + ""

    # print confirmation
    print(f"\nInfo:\n")
    print(f"infile: {input_file_path}")
    print(f"outfile: {output_file_path}")

    # defaul values
    default_output_type = "ogg"
    default_codec_a = "libvorbis"
    default_codec_v = None
    default_quality_a = "8"
    default_quality_v = None
    default_misc_args = "-vn"

    # important vars
    working_path = os.getcwd()
    script_path = os.path.realpath(__file__)
    
    
    

    # ui
    if get_yes_no_input("Do you want to continue?", default='yes'):
        print("you chose yes!")
    else:
        print("you chose no.")
        
    get_fzf_input(['hi', 'owo', 'uwu'], "--prompt 'select one: '")

if __name__ == '__main__':
    main()
