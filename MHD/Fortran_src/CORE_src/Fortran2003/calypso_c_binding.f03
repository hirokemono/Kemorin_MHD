!>@file   calypso_c_binding.f90
!!        module calypso_c_binding
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!@endverbatim
!
      module calypso_c_binding
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
!
        subroutine get_one_line_from_gz                                 &
     &           (num_buffer, num_word, nchara, line_buf)               &
     &            BIND(C, name = 'get_one_line_from_gz')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: num_buffer
        integer(C_int), intent(inout) :: num_word
        integer(C_int), intent(inout) :: nchara
        character(C_char), intent(inout) :: line_buf(*)
!
        end subroutine get_one_line_from_gz
!
!  ---------------------------------------------------------------------
!
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine get_one_line_from_gz_f03(len_buf, textbuf, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: len_buf
      character(len=1), intent(inout) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call get_one_line_from_gz(zbuf%len_buf, zbuf%num_word,            &
     &    zbuf%len_used, zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine get_one_line_from_gz_f03
!
!  ---------------------------------------------------------------------
!
      end module calypso_c_binding
