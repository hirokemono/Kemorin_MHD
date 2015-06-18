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
      implicit none
!
!
      type filter_mom_type
        real(kind=kreal),   pointer :: f_0(:)
!
        real(kind=kreal),   pointer :: f_x(:)
        real(kind=kreal),   pointer :: f_y(:)
        real(kind=kreal),   pointer :: f_z(:)
        real(kind=kreal),   pointer :: f_x2(:)
        real(kind=kreal),   pointer :: f_y2(:)
        real(kind=kreal),   pointer :: f_z2(:)
        real(kind=kreal),   pointer :: f_xy(:)
        real(kind=kreal),   pointer :: f_yz(:)
        real(kind=kreal),   pointer :: f_zx(:)
      end type filter_mom_type
!
      type filter_mom_diffs_type
        real(kind=kreal),   pointer :: df_0(:,:)
!
        real(kind=kreal),   pointer :: df_x(:,:)
        real(kind=kreal),   pointer :: df_y(:,:)
        real(kind=kreal),   pointer :: df_z(:,:)
        real(kind=kreal),   pointer :: df_x2(:,:)
        real(kind=kreal),   pointer :: df_y2(:,:)
        real(kind=kreal),   pointer :: df_z2(:,:)
        real(kind=kreal),   pointer :: df_xy(:,:)
        real(kind=kreal),   pointer :: df_yz(:,:)
        real(kind=kreal),   pointer :: df_zx(:,:)
      end type filter_mom_diffs_type
!
      type nod_mom_diffs_type
        type(filter_mom_type) :: moms
        type(filter_mom_diffs_type) :: diff
      end type nod_mom_diffs_type
!
      type ele_mom_diffs_type
        type(filter_mom_type) :: moms
        type(filter_mom_diffs_type) :: diff
        type(filter_mom_diffs_type) :: diff2
      end type ele_mom_diffs_type
!
      type gradient_filter_mom_type
        integer (kind = kint) :: nnod_fmom, nele_fmom
        integer (kind = kint) :: num_filter_moms
        type(nod_mom_diffs_type), pointer :: mom_nod(:)
        type(ele_mom_diffs_type), pointer :: mom_ele(:)
      end type gradient_filter_mom_type
!
      private :: alloc_moments_type, alloc_filter_mom_diffs_type
      private :: dealloc_moments_type, dealloc_filter_mom_diffs_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_moments_type(num_fmom, moms)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(filter_mom_type), intent(inout)  :: moms
!
!
      allocate( moms%f_0(num_fmom) )
!
      allocate( moms%f_x(num_fmom) )
      allocate( moms%f_y(num_fmom) )
      allocate( moms%f_z(num_fmom) )
!
      allocate( moms%f_x2(num_fmom) )
      allocate( moms%f_y2(num_fmom) )
      allocate( moms%f_z2(num_fmom) )
!
      allocate( moms%f_xy(num_fmom) )
      allocate( moms%f_yz(num_fmom) )
      allocate( moms%f_zx(num_fmom) )
!
      if (num_fmom .gt. 0) then
        moms%f_0 =  0.0d0
        moms%f_x =  0.0d0
        moms%f_y =  0.0d0
        moms%f_z =  0.0d0
        moms%f_x2 = 0.0d0
        moms%f_y2 = 0.0d0
        moms%f_z2 = 0.0d0
        moms%f_xy = 0.0d0
        moms%f_yz = 0.0d0
        moms%f_zx = 0.0d0
      end if 
!
      end subroutine alloc_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_mom_diffs_type(num_fmom, dmoms)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(filter_mom_diffs_type), intent(inout)  :: dmoms
!
!
      allocate( dmoms%df_0(num_fmom,3) )
!
      allocate( dmoms%df_x(num_fmom,3) )
      allocate( dmoms%df_y(num_fmom,3) )
      allocate( dmoms%df_z(num_fmom,3) )
!
      allocate( dmoms%df_x2(num_fmom,3) )
      allocate( dmoms%df_y2(num_fmom,3) )
      allocate( dmoms%df_z2(num_fmom,3) )
!
      allocate( dmoms%df_xy(num_fmom,3) )
      allocate( dmoms%df_yz(num_fmom,3) )
      allocate( dmoms%df_zx(num_fmom,3) )
!
      if (num_fmom .gt. 0) then
        dmoms%df_0 =  0.0d0
        dmoms%df_x =  0.0d0
        dmoms%df_y =  0.0d0
        dmoms%df_z =  0.0d0
        dmoms%df_x2 = 0.0d0
        dmoms%df_y2 = 0.0d0
        dmoms%df_z2 = 0.0d0
        dmoms%df_xy = 0.0d0
        dmoms%df_yz = 0.0d0
        dmoms%df_zx = 0.0d0
      end if 
!
      end subroutine alloc_filter_mom_diffs_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_moments_type(moms)
!
      type(filter_mom_type), intent(inout)  :: moms
!
!
      deallocate( moms%f_0 )
      deallocate( moms%f_x, moms%f_y, moms%f_z )
      deallocate( moms%f_x2, moms%f_y2, moms%f_z2 )
      deallocate( moms%f_xy, moms%f_yz, moms%f_zx )
!
      end subroutine dealloc_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_mom_diffs_type(dmoms)
!
      type(filter_mom_diffs_type), intent(inout)  :: dmoms
!
!
      deallocate( dmoms%df_0 )
      deallocate( dmoms%df_x, dmoms%df_y, dmoms%df_z )
      deallocate( dmoms%df_x2, dmoms%df_y2, dmoms%df_z2 )
      deallocate( dmoms%df_xy, dmoms%df_yz, dmoms%df_zx )
!
      end subroutine dealloc_filter_mom_diffs_type
!
!  ---------------------------------------------------------------------
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
        call alloc_moments_type                                         &
     &     (FEM_moms%nnod_fmom, FEM_moms%mom_nod(ifil)%moms)
        call alloc_filter_mom_diffs_type                                &
     &      (FEM_moms%nnod_fmom, FEM_moms%mom_nod(ifil)%diff)
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
        call dealloc_moments_type(FEM_moms%mom_nod(ifil)%moms)
        call dealloc_filter_mom_diffs_type(FEM_moms%mom_nod(ifil)%diff)
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
      integer(kind = kint) :: i
!
!
      FEM_moms%nele_fmom = nele
!
      allocate( FEM_moms%mom_ele(FEM_moms%num_filter_moms) )
      do i = 1, FEM_moms%num_filter_moms
        call alloc_moments_type                                         &
     &     (FEM_moms%nele_fmom, FEM_moms%mom_ele(i)%moms)
        call alloc_filter_mom_diffs_type                                &
     &     (FEM_moms%nele_fmom, FEM_moms%mom_ele(i)%diff)
        call alloc_filter_mom_diffs_type                                &
     &     (FEM_moms%nele_fmom, FEM_moms%mom_ele(i)%diff2)
      end do
!
      end subroutine alloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_moms_ele_type(FEM_moms)
!
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint) :: i
!
!
      do i = 1, FEM_moms%num_filter_moms
        call dealloc_moments_type(FEM_moms%mom_ele(i)%moms)
        call dealloc_filter_mom_diffs_type(FEM_moms%mom_ele(i)%diff)
        call dealloc_filter_mom_diffs_type(FEM_moms%mom_ele(i)%diff2)
      end do
      deallocate( FEM_moms%mom_ele )
!
      end subroutine dealloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      end module t_filter_moments
