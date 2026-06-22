!>@file   t_commute_filter_z.f90
!!        module t_commute_filter_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter in vertical direction
!!
!!@verbatim
!!      subroutine alloc_z_filter_mom_params(n_comp, z_commute)
!!      subroutine dealloc_z_filter_mom_params(z_commute)
!!        integer(kind = kint), intent(in) :: n_comp
!!        type(vart_fileter_params), intent(inout) :: z_commute
!!
!!      subroutine write_vart_filter_moments(id_file, z_commute)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(vart_fileter_params), intent(in) :: z_commute
!!@endverbatim
!!
      module t_commute_filter_z
!
      use m_precision
!
      implicit none
!
      type vart_fileter_params
        integer (kind = kint) :: ncomp_norm
        character(len=kchara), allocatable :: z_filter_mom_type(:)
        integer (kind = kint), allocatable :: kcomp_norm_z(:)
        real(kind = kreal), allocatable :: f_mom_z(:)
      end type vart_fileter_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_z_filter_mom_params(n_comp, z_commute)
!
      integer(kind = kint), intent(in) :: n_comp
      type(vart_fileter_params), intent(inout) :: z_commute
!
!
      z_commute%ncomp_norm = n_comp
      allocate(z_commute%kcomp_norm_z(z_commute%ncomp_norm))
      allocate(z_commute%f_mom_z(z_commute%ncomp_norm))
      allocate(z_commute%z_filter_mom_type(z_commute%ncomp_norm))
!
      if(z_commute%ncomp_norm .le. 0) return
      z_commute%kcomp_norm_z = 0
      z_commute%f_mom_z = 0.0d0
!
      end subroutine alloc_z_filter_mom_params
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_z_filter_mom_params(z_commute)
!
      type(vart_fileter_params), intent(inout) :: z_commute
!
!
      deallocate(z_commute%kcomp_norm_z)
      deallocate(z_commute%f_mom_z)
      deallocate(z_commute%z_filter_mom_type)
!
      end subroutine dealloc_z_filter_mom_params
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_vart_filter_moments(id_file, z_commute)
!
      integer(kind = kint), intent(in) :: id_file
      type(vart_fileter_params), intent(in) :: z_commute
!
      integer(kind = kint) :: i
!
!
      if(z_commute%ncomp_norm .le. 0) return
      write(id_file,'(a)')                                              &
     &      'i_comp, kcomp_norm_z, f_mom_z, z_filter_mom_type'
      do i = 1, z_commute%ncomp_norm
        write(id_file,'(2i6,1pe25.15e3,a)')                             &
     &        i, z_commute%kcomp_norm_z(i), z_commute%f_mom_z(i),       &
     &        trim(z_commute%z_filter_mom_type(i))
      end do
!
      end subroutine write_vart_filter_moments
!
!  ---------------------------------------------------------------------
!
      end module t_commute_filter_z
