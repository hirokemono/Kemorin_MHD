!>@file   t_z_filter_values.f90
!!        module t_z_filter_values
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter data in vertical direction
!!
!!@verbatim
!!      subroutine allocate_filter_values(nfilter6_1, zfil_v)
!!      subroutine deallocate_filter_values(zfil_v)
!!        integer(kind = kint), intent(in) :: nfilter6_1
!!        type(z_filter_values), intent(inout) :: zfil_v
!!      subroutine check_integrated_values(id_file, zfil_v)
!!        integer, intent(in) :: id_file
!!        type(z_filter_values), intent(in) :: zfil_v
!!@endverbatim
      module t_z_filter_values
!
      use m_precision
!
      implicit none
!
!
      type z_filter_values
        integer(kind = kint), private :: nfilter6_1
        real(kind = kreal), allocatable :: f_mom_full(:)
      end type z_filter_values
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_values(nfilter6_1, zfil_v)
!
      integer(kind = kint), intent(in) :: nfilter6_1
      type(z_filter_values), intent(inout) :: zfil_v
!
      zfil_v%nfilter6_1 = nfilter6_1
      allocate(zfil_v%f_mom_full(0:zfil_v%nfilter6_1))
!
      zfil_v%f_mom_full = 0.0d0
!
      end subroutine allocate_filter_values
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_values(zfil_v)
!
      type(z_filter_values), intent(inout) :: zfil_v
!
      deallocate(zfil_v%f_mom_full)
!
      end subroutine deallocate_filter_values
!
!  ---------------------------------------------------------------------
!
      subroutine check_integrated_values(id_file, zfil_v)
!
      integer, intent(in) :: id_file
      type(z_filter_values), intent(in) :: zfil_v
!
      write(id_file,*) 'f_mom_full'
      write(id_file,'(1p5e16.8)')                                       &
     &                          zfil_v%f_mom_full(0:zfil_v%nfilter6_1)
!
      end subroutine check_integrated_values
!
!  ---------------------------------------------------------------------
!
      end module t_z_filter_values
