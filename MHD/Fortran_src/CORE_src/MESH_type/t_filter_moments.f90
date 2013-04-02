!
!      module t_filter_moments
!
!     Written by H. Matsui on March, 2009
!     Modified by H. Matsui on Feb., 2012
!
!>    @brief Strucure for filter moments for SGS model
!
!      subroutine alloc_filter_moms_nod_type(nnod, FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of allocate_filter_moms_nod )
!      subroutine dealloc_filter_moms_nod_type(FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of deallocate_filter_moms_nod )
!      subroutine alloc_filter_moms_ele_type(nele, FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of allocate_filter_moms_ele )
!      subroutine dealloc_filter_moms_ele_type(FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of deallocate_filter_moms_ele )
!
!   data comparison
!
!      filter_x_nod(i,ifil)...  mom_nod(ifil)%moms%f_x(i)
!      filter_y_nod(i,ifil)...  mom_nod(ifil)%moms%f_y(i)
!      filter_z_nod(i,ifil)...  mom_nod(ifil)%moms%f_z(i)
!      filter_x2_nod(i,ifil)... mom_nod(ifil)%moms%f_x2(i)
!      filter_y2_nod(i,ifil)... mom_nod(ifil)%moms%f_y2(i)
!      filter_z2_nod(i,ifil)... mom_nod(ifil)%moms%f_z2(i)
!      filter_xy_nod(i,ifil)... mom_nod(ifil)%moms%f_xy(i)
!      filter_yz_nod(i,ifil)... mom_nod(ifil)%moms%f_yz(i)
!      filter_zx_nod(i,ifil)... mom_nod(ifil)%moms%f_zx(i)
!
!      filter_x_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_x(i,nd)
!      filter_y_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_y(i,nd)
!      filter_z_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_z(i,nd)
!      filter_x2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_x2(i,nd)
!      filter_y2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_y2(i,nd)
!      filter_z2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_z2(i,nd)
!      filter_xy_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_xy(i,nd)
!      filter_yz_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_yz(i,nd)
!      filter_zx_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_zx(i,nd)
!
!
!      filter_x_ele(i,ifil)...  mom_ele(ifil)%moms%f_x(i)
!      filter_y_ele(i,ifil)...  mom_ele(ifil)%moms%f_y(i)
!      filter_z_ele(i,ifil)...  mom_ele(ifil)%moms%f_z(i)
!      filter_x2_ele(i,ifil)... mom_ele(ifil)%moms%f_x2(i)
!      filter_y2_ele(i,ifil)... mom_ele(ifil)%moms%f_y2(i)
!      filter_z2_ele(i,ifil)... mom_ele(ifil)%moms%f_z2(i)
!      filter_xy_ele(i,ifil)... mom_ele(ifil)%moms%f_xy(i)
!      filter_yz_ele(i,ifil)... mom_ele(ifil)%moms%f_yz(i)
!      filter_zx_ele(i,ifil)... mom_ele(ifil)%moms%f_zx(i)
!
!      filter_x_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_x(i,nd)
!      filter_y_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_y(i,nd)
!      filter_z_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_z(i,nd)
!      filter_x2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_x2(i,nd)
!      filter_y2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_y2(i,nd)
!      filter_z2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_z2(i,nd)
!      filter_xy_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_xy(i,nd)
!      filter_yz_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_yz(i,nd)
!      filter_zx_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_zx(i,nd)
!
!      filter_x_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_x(i,nd)
!      filter_y_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_y(i,nd)
!      filter_z_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_z(i,nd)
!      filter_x2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_x2(i,nd)
!      filter_y2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_y2(i,nd)
!      filter_z2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_z2(i,nd)
!      filter_xy_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_xy(i,nd)
!      filter_yz_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_yz(i,nd)
!      filter_zx_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_zx(i,nd)
!         i:     element ID
!         nd:    direction of differenciate
!         ifil:  filter ID
!
!
      module t_filter_moments
!
      use m_precision
!
      use t_filter_elength
!
      implicit none
!
!
      type gradient_filter_mom_type
        integer (kind = kint) :: nnod_fmom, nele_fmom
        integer (kind = kint) :: num_filter_moms
        type(nod_mom_diffs_type), pointer :: mom_nod(:)
        type(ele_mom_diffs_type), pointer :: mom_ele(:)
      end type gradient_filter_mom_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_moms_nod_type(nnod, FEM_moms)
!
      integer(kind = kint), intent(in) :: nnod
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint) :: ifil
!
!
      FEM_moms%nnod_fmom = nnod
!
      allocate( FEM_moms%mom_nod(FEM_moms%num_filter_moms) )
      do ifil = 1, FEM_moms%num_filter_moms
        call alloc_nod_mom_diffs_type(FEM_moms%nnod_fmom,               &
     &      FEM_moms%mom_nod(ifil))
      end do
!
      end subroutine alloc_filter_moms_nod_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_moms_nod_type(FEM_moms)
!
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint) :: ifil
!
!
      do ifil = 1, FEM_moms%num_filter_moms
        call dealloc_nod_mom_diffs_type(FEM_moms%mom_nod(ifil))
      end do
      deallocate( FEM_moms%mom_nod )
!
      end subroutine dealloc_filter_moms_nod_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_moms_ele_type(nele, FEM_moms)
!
      integer(kind = kint), intent(in) :: nele
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint) :: ifil
!
!
      FEM_moms%nele_fmom = nele
!
      allocate( FEM_moms%mom_ele(FEM_moms%num_filter_moms) )
      do ifil = 1, FEM_moms%num_filter_moms
        call alloc_ele_mom_diffs_type(FEM_moms%nele_fmom,               &
     &      FEM_moms%mom_ele(ifil))
      end do
!
      end subroutine alloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_moms_ele_type(FEM_moms)
!
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint) :: ifil
!
!
      do ifil = 1, FEM_moms%num_filter_moms
        call dealloc_ele_mom_diffs_type(FEM_moms%mom_ele(ifil))
      end do
      deallocate( FEM_moms%mom_ele )
!
      end subroutine dealloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      end module t_filter_moments
