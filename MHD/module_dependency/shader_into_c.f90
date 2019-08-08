!
      module shader_into_c_source
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      subroutine append_each_shader                                     &
     &         (dirname, shader_file, c_prefix)
!
      character(len=1024), intent(in) :: dirname
      character(len=255), intent(in) :: shader_file
      character(len=255), intent(in) :: c_prefix
!
      character(len = 2048) :: full_path
      integer,parameter :: max_line_len = 4095
      character(len = max_line_len) linebuf
!
      character(len=512) :: shader_name
      character(len=512) :: func_name
      integer :: i, n
!
!
!      Set shader text name (replace period to underscore)
      write(*,*) 'len_trim(shader_file)', len_trim(shader_file)
      n = len_trim(shader_file)
      do i = 1, n
        if(shader_file(i:i) .eq. '.') then
          shader_name(i:i) = '_'
        else
          shader_name(i:i) = shader_file(i:i)
        end if
      end do
      do i = n+1, 512
        shader_name(i:i) = ' '
      end do
!
      n = len_trim(shader_name)
      write(func_name,'(a5)') 'load_'
      do i = 1, n
        func_name(i+5:i+5) = shader_name(i:i)
      end do
      do i = n+6, 512
        func_name(i:i) = ' '
      end do
!
      full_path = trim(dirname) // '/' // trim(c_prefix) // '.h'
      open(13, file = full_path, position='append')
      full_path = trim(dirname) // '/' // trim(c_prefix) // '.c'
      open(15, file = full_path, position='append')
      full_path = trim(dirname) // '/' // trim(shader_file)
      open(12, file = full_path, recl=max_line_len)
!
      write(13,'(3a)') 'char * ', trim(func_name), '();'
      write(15,'(3a)') 'char * ', trim(func_name), '(){'
      close(13)

      write(15,'(3a)') '    const char  ', trim(shader_name), '[]'
      write(15,'(a)')  '    = {'
      do
        read (12,'(a)',end=10) linebuf
        write(15,'(3a)') '        "', trim(linebuf), '\n"\'
      end do
  10  continue
      close(12)
!
      write(15,'(a)')  '        "\n"'
      write(15,'(a)')  '    };'
      write(15,'(a)')  '    '
      write(15,'(3a)') '    long n = strlen(', trim(shader_name), ');'
      write(15,'(a)')  '    char * src = alloc_string((int) n+1);'
      write(15,'(a)')  '    '
      write(15,'(3a)') '    strcpy(src, ', trim(shader_name), ');'
      write(15,'(a)')  '    return src;'
      write(15,'(a2,a1)')  '};', char(10)
      close(15)
!
      end subroutine append_each_shader
!
!   --------------------------------------------------------------------
!
      end module shader_into_c_source
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      program shader_into_c
!
      use shader_into_c_source
!
      implicit none
!
      character(len=255), allocatable :: filenames(:)
      character(len=255) :: c_file_prefix = 'shaders'
      character(len=255) :: c_header_file = 'shaders.h'
      character(len=255) :: c_source_file = 'shaders.c'
      character(len=1024) :: dirname
      character(len = 2048) :: full_path
!
      integer :: i, icount, num_file
!
!
      icount = iargc_kemo()
!
      if(icount .le. 2) then
        write(*,*)                                                      &
     &     'kemo_module_dep MAKEFILE SOURCE_DIRECTORY LIST_OF_SORCE'
        stop
      end if
!
      num_file = icount - 2
      allocate( filenames(num_file) )
!
      call getarg_k(1, c_file_prefix)
      call getarg_k(2, dirname)
      do i = 1, num_file
        call getarg_k((i+2), filenames(i))
      end do
!
      full_path = trim(dirname) // '/' // trim(c_file_prefix) // '.h'
       open(13, file = full_path)
       write(13,'(a)') '/*'
       write(13,'(a)') '//  Header of shader texts'
       write(13,'(a)') '//  Generated from shader files'
       write(13,'(a)') '*/'
       write(13,'(a)') '#ifndef shaders__'
       write(13,'(a)') '#define shaders__'
       write(13,'(a)') ''
       write(13,'(a)') '#include <string.h>'
       write(13,'(a)') '#include "kemoviewer.h"'
       write(13,'(a)') '#include "skip_comment_c.h"'
       write(13,'(a)') ''
       write(13,'(a)') '/* prototypes */'
       write(13,'(a)') ''
       close(13)
!
      full_path = trim(dirname) // '/' // trim(c_file_prefix) // '.c'
       open(15, file = full_path)
       write(15,'(a)') '/*'
       write(15,'(a)') '//  Source of shader texts'
       write(15,'(a)') '//  Generated from shader files'
       write(15,'(a)') '*/'
       write(15,'(a)') ''
       write(15,'(a)') '#include "shaders.h"'
       write(15,'(a)') ''
       close(15)
!
!
      do i = 1, num_file
        call append_each_shader(dirname, filenames(i), c_file_prefix)
      end do
!
      full_path = trim(dirname) // '/' // trim(c_file_prefix) // '.h'
       open(13, file = full_path, position='append')
       write(13,'(a)') ''
       write(13,'(a)') '#endif'
       close(13)
!
      end program shader_into_c
