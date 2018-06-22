!
!     module set_merged_restart_data
!
!      Written by H.Matsui
!
!!      subroutine rescale_4_magne(fld)
!!        type(phys_data), intent(inout) :: fld
!
      module set_merged_restart_data
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rescale_4_magne(fld)
!
      use m_control_param_merge
      use m_phys_labels
      use t_phys_data
!
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i, j, jst, jed, inod
!
!
      do i = 1, fld%num_phys
        jst = fld%istack_component(i-1) + 1
        jed = fld%istack_component(i)
        if (    fld%phys_name(i) .eq. fhd_vecp                          &
     &     .or. fld%phys_name(i) .eq. fhd_magne                         &
     &     .or. fld%phys_name(i) .eq. fhd_mag_potential                 &
     &     .or. fld%phys_name(i) .eq. fhd_pre_uxb                       &
     &     .or. fld%phys_name(i) .eq. fhd_chk_uxb) then
          do j = jst, jed
            do inod = 1, fld%n_point
              fld%d_fld(inod,j) = b_ratio * fld%d_fld(inod,j)
            end do
          end do
        end if
      end do
!
      end subroutine rescale_4_magne
!
!------------------------------------------------------------------
!
      end module set_merged_restart_data
