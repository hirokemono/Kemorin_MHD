!
!      module set_merged_udt_2_IO
!
!      Written by H. Matsui on Apr., 2010
!
!      subroutine link_merged_node_2_ucd_IO(ucd)
!      subroutine link_merged_ele_2_ucd_IO(ucd)
!      subroutine link_merged_field_2_udt_IO(ucd)
!        type(ucd_data), intent(inout) :: ucd
!
      module set_merged_udt_2_IO
!
      use m_precision
!
      use m_geometry_data_4_merge
      use t_ucd_data
      use set_ucd_data_to_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_node_2_ucd_IO(ucd)
!
      use set_ucd_data
!
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint_gl) :: inod
!
!
      ucd%nnod = merge_tbl%nnod_merged
      call allocate_ucd_node(ucd)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = merged%node%inod_global(inod)
        ucd%xx(inod,1) = merged%node%xx(inod,1)
        ucd%xx(inod,2) = merged%node%xx(inod,2)
        ucd%xx(inod,3) = merged%node%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine link_merged_node_2_ucd_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_ele_2_ucd_IO(ucd)
!
      use set_ucd_data
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: k1
!
!
      ucd%nele = merge_tbl%nele_merged
      ucd%nnod_4_ele = merged%ele%nnod_4_ele
      call allocate_ucd_ele(ucd)
!
!$omp parallel private(iele,k1)
!$omp do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = merged%ele%iele_global(iele)
      end do
!$omp end do nowait
!
      do k1 = 1, ucd%nnod_4_ele
!$omp do
        do iele = 1, ucd%nele
          ucd%ie(iele,k1) = merged%ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine link_merged_ele_2_ucd_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_field_2_udt_IO(ucd)
!
      use set_ucd_data
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: inod
      integer(kind = kint) :: nd
!
!
      ucd%nnod =      merge_tbl%nnod_merged
      ucd%num_field = merged_fld%num_phys_viz
      ucd%ntot_comp = merged_fld%ntot_phys_viz
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      do nd = 1, ucd%num_field
        ucd%num_comp(nd) =  merged_fld%num_component(nd)
        ucd%phys_name(nd) = merged_fld%phys_name(nd)
      end do
!
!$omp parallel private(inod,nd)
      do nd = 1, ucd%ntot_comp
!$omp do
        do inod = 1, ucd%nnod
          ucd%d_ucd(inod,nd) = merged_fld%d_fld(inod,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine link_merged_field_2_udt_IO
!
! -----------------------------------------------------------------------
!
      end module set_merged_udt_2_IO
