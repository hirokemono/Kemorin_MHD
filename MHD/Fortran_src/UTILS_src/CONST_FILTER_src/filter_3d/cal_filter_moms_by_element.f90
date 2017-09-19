!cal_filter_moms_by_element.f90
!     module cal_filter_moms_by_element
!
!     Written by H. Matsui on Apr., 2008
!
!!      subroutine cal_filter_moments_on_node_1st                       &
!!     &         (nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl, tbl_crs, &
!!     &          m_lump, FEM_elen, mass, fem_wk, f_l)
!!      subroutine cal_filter_moments_on_ele(FEM_elen)
!
!
      module cal_filter_moms_by_element
!
      use m_precision
!
      use m_reference_moments
      use m_phys_constants
!
      use t_table_FEM_const
      use t_finite_element_mat
      use t_fem_gauss_int_coefs
!
      implicit none
!
      private :: cal_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_node_1st                         &
     &         (nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl, tbl_crs,   &
     &          m_lump, FEM_elen, mass, fem_wk, f_l)
!
      use t_geometry_data
      use t_jacobians
      use t_filter_elength
      use t_comm_table
      use t_crs_matrix
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(CRS_matrix), intent(inout) :: mass
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call cal_filter_moments_on_node                                   &
     &   (nod_comm, node, ele, g_FEM, jac_3d,                           &
     &    rhs_tbl, tbl_crs, m_lump, FEM_elen%nnod_filter_mom,           &
     &    FEM_elen%elen_nod%moms%f_x2, FEM_elen%elen_nod%moms%f_y2,     &
     &    FEM_elen%elen_nod%moms%f_z2, FEM_elen%elen_nod%moms%f_xy,     &
     &    FEM_elen%elen_nod%moms%f_yz, FEM_elen%elen_nod%moms%f_zx,     &
     &    mass, fem_wk, f_l)
!
      end subroutine cal_filter_moments_on_node_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_ele(dxi_ele, FEM_elen)
!
      use m_machine_parameter
      use t_filter_elength
      use t_filter_dxdxi
      use cal_1d_moments_4_fliter
      use set_filter_moments_3d
!
      type(dxdxi_direction_type), intent(in) :: dxi_ele
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      integer(kind = kint) :: iele
!
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_moments_order'
      call s_set_moments_order
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_table_4_moments'
      call s_set_table_4_moments
      if (iflag_debug.eq.1)  write(*,*) 's_cal_1d_moments_4_filter'
      call s_cal_1d_moments_4_filter(FEM_elen)
!
!      if (iflag_debug.eq.1)                                            &
!     & write(*,*) 's_set_seeds_moments', FEM_elen%nele_filter_mom,     &
!     &             num_order_3d
      do iele = 1, FEM_elen%nele_filter_mom
        call s_set_seeds_moments                                        &
     &      (dxi_ele%dx%df_dxi(iele), dxi_ele%dx%df_dei(iele),          &
     &       dxi_ele%dx%df_dzi(iele), dxi_ele%dy%df_dxi(iele),          &
     &       dxi_ele%dy%df_dei(iele), dxi_ele%dy%df_dzi(iele),          &
     &       dxi_ele%dz%df_dxi(iele), dxi_ele%dz%df_dei(iele),          &
     &       dxi_ele%dz%df_dzi(iele))
!
        seed_moments_ele(iele,1:num_order_3d)                           &
     &      = seed_moments(1:num_order_3d)
      end do
!
      end subroutine cal_filter_moments_on_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_node                             &
     &         (nod_comm, node, ele, g_FEM, jac_3d,                     &
     &          rhs_tbl, tbl_crs, m_lump, nnod_filter_mom,              &
     &          elen_dx2_nod,  elen_dy2_nod,  elen_dz2_nod,             &
     &          elen_dxdy_nod, elen_dydz_nod, elen_dzdx_nod,            &
     &          mass, fem_wk, f_l)
!
      use m_ctl_params_4_gen_filter
      use filter_moments_send_recv
      use int_vol_elesize_on_node
      use nod_phys_send_recv
!
      use t_geometry_data
      use t_jacobians
      use t_comm_table
      use t_crs_matrix
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
!
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: nnod_filter_mom
      real(kind=kreal), intent(in) :: elen_dx2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dy2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dz2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dxdy_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dydz_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dzdx_nod(nnod_filter_mom)
!
      type(CRS_matrix), intent(inout) :: mass
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: inod, n, im_x, im_y, im_z
!
!
      do n = 1, num_order_3d
!
        im_x = iorder_mom_3d(n,1)
        im_y = iorder_mom_3d(n,2)
        im_z = iorder_mom_3d(n,3)
!
        if ( mod((im_x+im_y+im_z),2) .eq. 1) then
          seed_moments_nod(1:nnod_filter_mom,n) = 0.0d0
        else if(im_x.eq.0 .and. im_y.eq.0 .and. im_z.eq.0) then
          seed_moments_nod(1:nnod_filter_mom,n) = 1.0d0
!
        else if(im_x.eq.2 .and. im_y.eq.0 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dx2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.2 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dy2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.0 .and. im_z.eq.2) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dz2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
!
        else if(im_x.eq.1 .and. im_y.eq.1 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dxdy_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.1 .and. im_z.eq.1) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dydz_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.1 .and. im_y.eq.0 .and. im_z.eq.1) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dzdx_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
!
        else
!
          call int_dx_ele2_node(nod_comm, node, ele, g_FEM, jac_3d,     &
     &        rhs_tbl, tbl_crs, m_lump, itype_mass_matrix,              &
     &        mass, seed_moments_ele(1,n),                              &
     &        seed_moments_nod(1,n), fem_wk, f_l)
          call nod_scalar_send_recv                                     &
     &       (node%numnod, nod_comm, seed_moments_nod(1,n) )
        end if
!
      end do
!
      end subroutine cal_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      end module cal_filter_moms_by_element
