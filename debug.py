from proud.pybackend import PyBackEnd

filename = "example_code/debug.prd"
end = PyBackEnd()
end.verbose = True
# try:
end.main(filename, filename + '.sij')


