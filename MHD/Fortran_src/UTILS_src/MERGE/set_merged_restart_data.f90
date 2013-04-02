!
!     module set_merged_restart_data
!
!      Written by H.Matsui
!
!      subroutine set_restart_data_2_merge(ip)
!      subroutine set_new_restart_data(ip)
!      subroutine rescale_4_magne
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
      subroutine set_restart_data_2_merge(ip)
!
      use m_geometry_data_4_merge
      use m_field_data_IO
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: i, j, inod
!
        do j = 1, ntot_phys_data_IO
          do inod = 1, subdomain(ip)%node%internal_node
            i = subdomain(ip)%node%inod_global(inod)
            merged_fld%d_fld(i,j) = phys_data_IO(inod,j)
          end do
        end do
!
      end subroutine set_restart_data_2_merge
!
!------------------------------------------------------------------
!
      subroutine set_new_restart_data(ip)
!
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_field_data_IO
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: i, j, inod
!
!
      do j = 1, ntot_phys_data_IO
        do inod = 1, subdomains_2(ip)%node%numnod
          i = subdomains_2(ip)%node%inod_global(inod)
          phys_data_IO(inod,j) = merged_fld%d_fld(i,j)
        end do
      end do
!
      end subroutine set_new_restart_data
!
!------------------------------------------------------------------
!
      subroutine rescale_4_magne
!
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_phys_labels
!
      integer(kind = kint) :: i, j, jst, jed, inod
!
!
      do i = 1, merged_fld%num_phys
        jst = merged_fld%istack_component(i-1) + 1
        jed = merged_fld%istack_component(i)
        if (    merged_fld%phys_name(i) .eq. fhd_vecp                   &
     &     .or. merged_fld%phys_name(i) .eq. fhd_magne                  &
     &     .or. merged_fld%phys_name(i) .eq. fhd_mag_potential          &
     &     .or. merged_fld%phys_name(i) .eq. fhd_pre_uxb                &
     &     .or. merged_fld%phys_name(i) .eq. fhd_chk_uxb) then
          do j = jst, jed
            do inod = 1, merge_tbl%nnod_merged
              merged_fld%d_fld(inod,j)                                  &
     &              = b_ratio * merged_fld%d_fld(inod,j)
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
