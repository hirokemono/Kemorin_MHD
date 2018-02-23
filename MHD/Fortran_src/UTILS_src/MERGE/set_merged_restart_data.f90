!
!     module set_merged_restart_data
!
!      Written by H.Matsui
!
!!      subroutine set_restart_data_2_merge(ip, fld_IO, merged_fld)
!!        type(mesh_geometry), intent(in) :: subdomain
!!        type(field_IO), intent(in) :: fld_IO
!!        type(phys_data), intent(inout) :: merged_fld
!!      subroutine set_new_restart_data                                 &
!!     &         (merged_fld, subdomains_2, fld_IO)
!!        type(phys_data), intent(in) :: merged_fld
!!        type(mesh_geometry), intent(in) :: subdomains_2
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine rescale_4_magne(merge_tbl, merged_fld)
!!        type(merged_stacks), intent(in) :: merge_tbl
!!        type(phys_data), intent(inout) :: merged_fld
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
      subroutine set_restart_data_2_merge(subdomain, fld_IO, merged_fld)
!
      use t_mesh_data
      use t_phys_data
      use t_field_data_IO
!
      type(mesh_geometry), intent(in) :: subdomain
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: merged_fld
!
      integer(kind = kint) :: j, inod
      integer(kind = kint_gl) :: inod_gl
!
!
      do j = 1, fld_IO%ntot_comp_IO
        do inod = 1, subdomain%node%internal_node
          inod_gl = subdomain%node%inod_global(inod)
          merged_fld%d_fld(inod_gl,j) = fld_IO%d_IO(inod,j)
        end do
      end do
!
      end subroutine set_restart_data_2_merge
!
!------------------------------------------------------------------
!
      subroutine set_new_restart_data                                   &
     &         (merged_fld, subdomains_2, fld_IO)
!
      use t_mesh_data
      use t_phys_data
      use t_field_data_IO
!
      type(phys_data), intent(in) :: merged_fld
      type(mesh_geometry), intent(in) :: subdomains_2
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: j, inod
      integer(kind = kint_gl) :: inod_gl
!
!
      do j = 1, fld_IO%ntot_comp_IO
        do inod = 1, subdomains_2%node%numnod
          inod_gl = subdomains_2%node%inod_global(inod)
          fld_IO%d_IO(inod,j) = merged_fld%d_fld(inod_gl,j)
        end do
      end do
!
      end subroutine set_new_restart_data
!
!------------------------------------------------------------------
!
      subroutine rescale_4_magne(merge_tbl, merged_fld)
!
      use m_control_param_merge
      use m_phys_labels
      use t_phys_data
      use t_merged_geometry_data
!
      type(merged_stacks), intent(in) :: merge_tbl
      type(phys_data), intent(inout) :: merged_fld
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
