!>@file   set_num_domain_each_dir.f90
!!@brief  module set_num_domain_each_dir
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2005
!
!>@brief  Set parameters for partitioning
!!
!!@verbatim
!!      subroutine set_control_EQ_XYZ(ndomain_section_ctl,              &
!!     &                              new_nprocs, ndomain_eb)
!!      subroutine set_control_EQ_SPH(ndomain_section_ctl,              &
!!     &                              new_nprocs, ndomain_eb)
!!        type(ctl_array_ci), intent(in) :: ndomain_section_ctl
!!@endverbatim
!
      module set_num_domain_each_dir
!
      use m_precision
      use m_constants
!
      use t_control_array_charaint
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_EQ_XYZ(ndomain_section_ctl,                &
     &                              new_nprocs, ndomain_eb)
!
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: ndomain_section_ctl
      integer(kind = kint), intent(inout) :: new_nprocs
      integer(kind = kint), intent(inout) :: ndomain_eb(3)
!
      integer(kind = kint) :: i
!
!
      ndomain_eb(1:3) = 1
      do i = 1, ndomain_section_ctl%num
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'X')               &
     &        ) ndomain_eb(1) = ndomain_section_ctl%ivec(i)
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Y')               &
     &        ) ndomain_eb(2) = ndomain_section_ctl%ivec(i)
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Z')               &
     &        ) ndomain_eb(3) = ndomain_section_ctl%ivec(i)
      end do
!
      new_nprocs = ndomain_eb(1) * ndomain_eb(2) * ndomain_eb(3)
!
      end subroutine set_control_EQ_XYZ
!
! -----------------------------------------------------------------------
!
      subroutine set_control_EQ_SPH(ndomain_section_ctl,                &
     &                              new_nprocs, ndomain_eb)
!
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: ndomain_section_ctl
!
      integer(kind = kint), intent(inout) :: new_nprocs
      integer(kind = kint), intent(inout) :: ndomain_eb(3)
!
      integer(kind = kint) :: i
!
!
        ndomain_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(      cmp_no_case(ndomain_section_ctl%c_tbl(i), 'R')       &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radius')  &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radial')  &
     &       ) then
            ndomain_eb(1) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Theta')   &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Meridional') &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Elevation')  &
     &        ) then
            ndomain_eb(2) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Phi')     &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i),'Longitudinal') &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Azimuth')     &
     &        ) then
            ndomain_eb(3) = ndomain_section_ctl%ivec(i)
          end if
        end do
        new_nprocs = ndomain_eb(1) * ndomain_eb(2) * ndomain_eb(3)
!
      end subroutine set_control_EQ_SPH
!
! -----------------------------------------------------------------------
!
      end module set_num_domain_each_dir
