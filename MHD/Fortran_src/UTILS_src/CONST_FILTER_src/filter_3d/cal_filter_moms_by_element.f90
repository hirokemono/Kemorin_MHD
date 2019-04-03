!cal_filter_moms_by_element.f90
!     module cal_filter_moms_by_element
!
!     Written by H. Matsui on Apr., 2008
!
!!      subroutine cal_filter_moments_on_node_1st(nod_comm, node, ele,  &
!!     &          g_FEM, jac_3d, rhs_tbl, tbl_crs, m_lump, fil_elist,   &
!!     &          FEM_elen, gfil_p, mass, fem_wk, f_l, ref_m)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data),    intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(CRS_matrix), intent(inout) :: mass
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(reference_moments), intent(inout) :: ref_m
!!      subroutine cal_filter_moments_on_ele                            &
!!     &         (gfil_p, dxi_ele, FEM_elen, ref_m)
!!        type(ctl_params_4_gen_filter), intent(in) :: gfil_p
!!        type(dxdxi_direction_type), intent(in) :: dxi_ele
!!        type(gradient_model_data_type), intent(inout) :: FEM_elen
!!        type(reference_moments), intent(inout) :: ref_m
!
!
      module cal_filter_moms_by_element
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_table_FEM_const
      use t_finite_element_mat
      use t_fem_gauss_int_coefs
      use t_reference_moments
      use t_element_list_4_filter
      use t_ctl_params_4_gen_filter
!
      implicit none
!
      type work_for_moment_calc
        integer(kind = kint) :: ntot_power
        integer(kind = kint), allocatable :: istack_power(:)
        integer(kind = kint), allocatable :: itbl_power(:,:)
!
        real(kind = kreal), allocatable :: coef_x(:)
        real(kind = kreal), allocatable :: coef_y(:)
        real(kind = kreal), allocatable :: coef_z(:)
!
        integer(kind = kint), allocatable :: ipower_x_xi(:)
        integer(kind = kint), allocatable :: ipower_x_ei(:)
        integer(kind = kint), allocatable :: ipower_x_zi(:)
        integer(kind = kint), allocatable :: ipower_y_xi(:)
        integer(kind = kint), allocatable :: ipower_y_ei(:)
        integer(kind = kint), allocatable :: ipower_y_zi(:)
        integer(kind = kint), allocatable :: ipower_z_xi(:)
        integer(kind = kint), allocatable :: ipower_z_ei(:)
        integer(kind = kint), allocatable :: ipower_z_zi(:)
      end type work_for_moment_calc
!
      private :: alloc_istack_power, alloc_work_4_filter_moms
      private :: dealloc_work_4_filter_moms
      private :: cal_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_istack_power(max_num_order_1d, ref_wk)
!
      integer(kind = kint), intent(in) :: max_num_order_1d
      type(work_for_moment_calc), intent(inout) :: ref_wk
!
!
      allocate( ref_wk%istack_power(-1:max_num_order_1d) )
      ref_wk%istack_power = -1
!
      end subroutine alloc_istack_power
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_filter_moms(ref_wk)
!
      type(work_for_moment_calc), intent(inout) :: ref_wk
!
!
      allocate( ref_wk%itbl_power(3,0:ref_wk%ntot_power) )
      ref_wk%itbl_power = 0
!
      allocate( ref_wk%coef_x(0:ref_wk%ntot_power) )
      allocate( ref_wk%coef_y(0:ref_wk%ntot_power) )
      allocate( ref_wk%coef_z(0:ref_wk%ntot_power) )
!
      allocate( ref_wk%ipower_x_xi(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_x_ei(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_x_zi(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_y_xi(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_y_ei(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_y_zi(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_z_xi(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_z_ei(0:ref_wk%ntot_power) )
      allocate( ref_wk%ipower_z_zi(0:ref_wk%ntot_power) )
!
      ref_wk%coef_x = 0.0d0
      ref_wk%coef_y = 0.0d0
      ref_wk%coef_z = 0.0d0
!
      ref_wk%ipower_x_xi = 0
      ref_wk%ipower_x_ei = 0
      ref_wk%ipower_x_zi = 0
      ref_wk%ipower_y_xi = 0
      ref_wk%ipower_y_ei = 0
      ref_wk%ipower_y_zi = 0
      ref_wk%ipower_z_xi = 0
      ref_wk%ipower_z_ei = 0
      ref_wk%ipower_z_zi = 0
!
      end subroutine alloc_work_4_filter_moms
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_filter_moms(ref_wk)
!
      type(work_for_moment_calc), intent(inout) :: ref_wk
!
!
      deallocate( ref_wk%coef_x, ref_wk%coef_y, ref_wk%coef_z )
!
      deallocate( ref_wk%ipower_x_xi )
      deallocate( ref_wk%ipower_x_ei )
      deallocate( ref_wk%ipower_x_zi )
      deallocate( ref_wk%ipower_y_xi )
      deallocate( ref_wk%ipower_y_ei )
      deallocate( ref_wk%ipower_y_zi )
      deallocate( ref_wk%ipower_z_xi )
      deallocate( ref_wk%ipower_z_ei )
      deallocate( ref_wk%ipower_z_zi )
!
      deallocate(ref_wk%istack_power, ref_wk%itbl_power)
!
      end subroutine dealloc_work_4_filter_moms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_node_1st(nod_comm, node, ele,    &
     &          g_FEM, jac_3d, rhs_tbl, tbl_crs, m_lump, fil_elist,     &
     &          FEM_elen, gfil_p, mass, fem_wk, f_l, ref_m)
!
      use t_geometry_data
      use t_jacobians
      use t_filter_elength
      use t_comm_table
      use t_crs_matrix
      use t_reference_moments
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(element_list_4_filter), intent(in) :: fil_elist
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(CRS_matrix), intent(inout) :: mass
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(reference_moments), intent(inout) :: ref_m
!
!
      call cal_filter_moments_on_node                                   &
     &   (nod_comm, node, ele, g_FEM, jac_3d, rhs_tbl,                  &
     &    tbl_crs, m_lump, fil_elist, FEM_elen%nnod_filter_mom,         &
     &    FEM_elen%nele_filter_mom, ref_m%max_num_order_1d,             &
     &    ref_m%num_order_3d, ref_m%iorder_mom_3d,                      &
     &    ref_m%ref_moments_1d, ref_m%seed_moments_ele,                 &
     &    FEM_elen%elen_nod%moms%f_x2, FEM_elen%elen_nod%moms%f_y2,     &
     &    FEM_elen%elen_nod%moms%f_z2, FEM_elen%elen_nod%moms%f_xy,     &
     &    FEM_elen%elen_nod%moms%f_yz, FEM_elen%elen_nod%moms%f_zx,     &
     &    gfil_p, mass, fem_wk, f_l, ref_m%seed_moments_nod)
!
      end subroutine cal_filter_moments_on_node_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_ele                              &
     &         (gfil_p, dxi_ele, FEM_elen, ref_m)
!
      use t_filter_elength
      use t_filter_dxdxi
      use cal_1d_moments_4_fliter
      use set_filter_moments_3d
!
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(dxdxi_direction_type), intent(in) :: dxi_ele
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(reference_moments), intent(inout) :: ref_m
!
      integer(kind = kint) :: iele
!
      type(work_for_moment_calc) :: ref_wk
!
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_moments_order'
      call s_set_moments_order                                          &
     &   (gfil_p%num_moments_order, gfil_p%mom_order,                   &
     &    ref_m%max_num_order_3d, ref_m%num_order_3d,                   &
     &    ref_m%id_moments, ref_m%iorder_mom_3d)
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_table_4_moments'
      call alloc_istack_power(ref_m%max_num_order_1d, ref_wk)
      call count_table_4_moments(ref_m%max_num_order_1d,                &
     &    ref_wk%istack_power, ref_wk%ntot_power)
      call alloc_work_4_filter_moms(ref_wk)
      call s_set_table_4_moments(ref_m%max_num_order_1d,                &
     &    ref_wk%istack_power, ref_wk%ntot_power, ref_wk%itbl_power)
      call alloc_coef_4_filter_moms(ref_m)
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_1d_moments_4_filter'
      call s_cal_1d_moments_4_filter                                    &
     &   (gfil_p%num_ref_filter, gfil_p%iref_filter_type,               &
     &    FEM_elen, ref_m)
!
!      if (iflag_debug.eq.1)                                            &
!     & write(*,*) 's_set_seeds_moments', FEM_elen%nele_filter_mom,     &
!     &             ref_m%num_order_3d
      do iele = 1, FEM_elen%nele_filter_mom
        call s_set_seeds_moments                                        &
     &     (dxi_ele%dx%df_dxi(iele), dxi_ele%dx%df_dei(iele),           &
     &      dxi_ele%dx%df_dzi(iele), dxi_ele%dy%df_dxi(iele),           &
     &      dxi_ele%dy%df_dei(iele), dxi_ele%dy%df_dzi(iele),           &
     &      dxi_ele%dz%df_dxi(iele), dxi_ele%dz%df_dei(iele),           &
     &      dxi_ele%dz%df_dzi(iele), ref_m%max_num_order_1d,            &
     &      ref_wk%istack_power, ref_wk%ntot_power, ref_wk%itbl_power,  &
     &      ref_m%num_order_3d, ref_m%iorder_mom_3d,                    &
     &      ref_m%ref_moments_1d, ref_m%seed_moments,                   &
     &      ref_wk%coef_x, ref_wk%coef_y, ref_wk%coef_z,                &
     &      ref_wk%ipower_x_xi, ref_wk%ipower_x_ei, ref_wk%ipower_x_zi, &
     &      ref_wk%ipower_y_xi, ref_wk%ipower_y_ei, ref_wk%ipower_y_zi, &
     &      ref_wk%ipower_z_xi, ref_wk%ipower_z_ei, ref_wk%ipower_z_zi)
!
        ref_m%seed_moments_ele(iele,1:ref_m%num_order_3d)               &
     &      = ref_m%seed_moments(1:ref_m%num_order_3d)
      end do
      call dealloc_work_4_filter_moms(ref_wk)
!
      end subroutine cal_filter_moments_on_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_node(nod_comm, node, ele,        &
     &          g_FEM, jac_3d, rhs_tbl, tbl_crs, m_lump, fil_elist,     &
     &          nnod_filter_mom, nele_filter_mom,                       &
     &          max_num_order_1d, num_order_3d, iorder_mom_3d,          &
     &          ref_moments_1d, seed_moments_ele,                       &
     &          elen_dx2_nod,  elen_dy2_nod,  elen_dz2_nod,             &
     &          elen_dxdy_nod, elen_dydz_nod, elen_dzdx_nod,            &
     &          gfil_p, mass, fem_wk, f_l, seed_moments_nod)
!
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
      type(element_list_4_filter), intent(in) :: fil_elist
!
      integer(kind = kint), intent(in) :: nnod_filter_mom
      integer(kind = kint), intent(in) :: nele_filter_mom
      integer(kind = kint), intent(in) :: max_num_order_1d
      integer(kind = kint), intent(in) :: num_order_3d
      integer(kind = kint), intent(in) :: iorder_mom_3d(num_order_3d,3)
      real(kind = kreal), intent(in)                                    &
     &           :: ref_moments_1d(0:3*max_num_order_1d)
      real(kind = kreal), intent(in)                                    &
     &           :: seed_moments_ele(nele_filter_mom,num_order_3d)
      real(kind=kreal), intent(in) :: elen_dx2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dy2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dz2_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dxdy_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dydz_nod(nnod_filter_mom)
      real(kind=kreal), intent(in) :: elen_dzdx_nod(nnod_filter_mom)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(CRS_matrix), intent(inout) :: mass
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      real(kind = kreal), intent(inout)                                 &
     &               :: seed_moments_nod(nnod_filter_mom,num_order_3d)
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
     &        rhs_tbl, tbl_crs, m_lump, fil_elist, gfil_p,              &
     &        mass, seed_moments_ele(1,n), seed_moments_nod(1,n),       &
     &        fem_wk, f_l)
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
