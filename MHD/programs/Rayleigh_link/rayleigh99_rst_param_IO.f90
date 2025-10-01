!>@file   rayleigh99_rst_param_IO.f90
!!@brief  module rayleigh99_rst_param_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine read_rayleigh99_restart_params(dir, i_step, ra_rst)
!!        integer(kind = kint), intent(in) :: i_step
!1        character(len = kchara), intent(in) :: dir
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!      character(len = kchara) function set_rayleigh99_file_name       &
!!     &                               (dir, int_id, postfix)
!!        integer(kind = kint), intent(in) :: int_id
!!        character(len=kchara), intent(in) :: dir, postfix
!!@endverbatim
!
      module rayleigh99_rst_param_IO
!
      use m_precision
      use t_rayleigh_restart_IO
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh99_restart_params(dir, i_step, ra_rst)
!
      use byte_swap_f
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      integer, parameter :: id_file = 15
      character(len = kchara) :: file_name
      integer :: i4_tmp(32), i
      real(kind = kreal) :: r_tmp(1)
      integer(kind = kint_gl) :: l8_byte
!
!
      write(*,*) 'i_step', i_step
      file_name =  set_rayleigh99_file_name(dir, i_step, paramchar)
      write(*,*) 'read Rayleigh checkpoint paramter file: ',            &
     &          trim(file_name)
        open(id_file, FILE=file_name, STATUS='OLD',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
!
      ra_rst%iflag_swap = iendian_KEEP
      read(id_file) i4_tmp(1)
      if(i4_tmp(1) .ne. 4) ra_rst%iflag_swap = iendian_FLIP
!
      read(id_file) ra_rst%nri_org, i4_tmp(1)
      read(id_file) i4_tmp(1), ra_rst%iflag_rtype, i4_tmp(1)
      read(id_file) i4_tmp(1), ra_rst%ltr_org, i4_tmp(1)
!
      read(id_file) i4_tmp(1), ra_rst%dt_org, i4_tmp(1)
      read(id_file) i4_tmp(1), ra_rst%dt_new, i4_tmp(1)
!      read(id_file) i4_tmp(1), ra_rst%new_dt_org, i4_tmp(1)
!
      if(ra_rst%iflag_swap .eq. iendian_FLIP) then
        l8_byte = 1
!
        i4_tmp(1) = ra_rst%i_version_from_file 
        call byte_swap_int4_f(l8_byte, i4_tmp)
        ra_rst%i_version_from_file = i4_tmp(1)
!
        i4_tmp(1) = ra_rst%nri_org 
        call byte_swap_int4_f(l8_byte, i4_tmp)
        ra_rst%nri_org = i4_tmp(1)
!
        i4_tmp(1) = ra_rst%iflag_rtype 
        call byte_swap_int4_f(l8_byte, i4_tmp)
        ra_rst%iflag_rtype = i4_tmp(1)
!
        i4_tmp(1) = ra_rst%ltr_org 
        call byte_swap_int4_f(l8_byte, i4_tmp)
        ra_rst%ltr_org = i4_tmp(1)
!
        r_tmp(1) = ra_rst%dt_org
        call byte_swap_real_f(l8_byte, r_tmp)
        ra_rst%dt_org = r_tmp(1)
!
        r_tmp(1) = ra_rst%dt_new
        call byte_swap_real_f(l8_byte, r_tmp)
        ra_rst%dt_new = r_tmp(1)
!        r_tmp(1) = ra_rst%new_dt_org
!        call byte_swap_real_f(l8_byte, ra_rst%new_dt_org)
!        ra_rst%new_dt_org = r_tmp(1)
      end if
      write(*,*) 'ra_rst%nri_org', ra_rst%nri_org
      write(*,*) 'ra_rst%iflag_rtype', ra_rst%iflag_rtype
      write(*,*) 'ra_rst%ltr_org', ra_rst%ltr_org
!
      write(*,*) 'ra_rst%dt_org', ra_rst%dt_org
      write(*,*) 'ra_rst%dt_new', ra_rst%dt_new
!
      call alloc_rayleigh_radial_grid(ra_rst)
!
      read(id_file)                                                     &
     &          i4_tmp(1), ra_rst%r_org(1:ra_rst%nri_org), i4_tmp(1)
      read(id_file) i4_tmp(1), ra_rst%time_org, i4_tmp(1)
!
      close(id_file)
!
      if(ra_rst%iflag_swap .eq. iendian_FLIP) then
        l8_byte = ra_rst%nri_org
        call byte_swap_real_f(l8_byte, ra_rst%r_org)
        l8_byte = 1
        call byte_swap_real_f(l8_byte, r_tmp)
        ra_rst%time_org = r_tmp(1)
      end if
!
      end subroutine read_rayleigh99_restart_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh99_file_name         &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh99_file_name,1000)                              &
     &                        trim(dir), int_id, trim(postfix)
 1000 format(a, '/', i8.8, '_', a)
!
      end function set_rayleigh99_file_name
!
!-----------------------------------------------------------------------
!
      end module rayleigh99_rst_param_IO
