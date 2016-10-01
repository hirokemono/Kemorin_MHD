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
!      subroutine alloc_filter_mom_ele_items(nele, n_filter, mom_ele)
!      subroutine dealloc_filter_mom_ele_items(n_filter, mom_ele)
!        integer(kind = kint), intent(in) :: nele, n_filter
!        type(filter_mom_diffs_type), intent(inout) :: mom_ele
!      subroutine alloc_filter_moms_ele_type(nele, FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of allocate_filter_moms_ele )
!      subroutine dealloc_filter_moms_ele_type(FEM_moms)
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!       (substitution of deallocate_filter_moms_ele )
!
!      subroutine copy_moments_each_point                               &
!     &         (i_org, mom_org, i_tgt, mom_tgt)
!      subroutine copy_moments_type(num, mom_org, mom_tgt)
!        type(filter_mom_type), intent(in) :: mom_org
!        type(filter_mom_type), intent(inout) :: mom_tgt
!      subroutine copy_mom_diffs_each_point                             &
!     &         (i_org, mom_org, i_tgt, mom_tgt)
!        type(filter_mom_diffs_type), intent(in) :: mom_org
!        type(filter_mom_diffs_type), intent(inout) :: mom_tgt
!
!      subroutine copy_filter_moms_ele                                  &
!     &         (nele, n_filter, org_mom_ele, tgt_mom_ele)
!        integer(kind = kint), intent(in) :: nele, n_filter
!        type(ele_mom_diffs_type), intent(in) ::    org_mom_ele(n_filter)
!        type(ele_mom_diffs_type), intent(inout) :: tgt_mom_ele(n_filter)
!      subroutine copy_filter_moms_nod_type(FEM_moms_org, FEM_moms_tgt)
!        type(gradient_filter_mom_type), intent(in) ::    FEM_moms_org
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms_tgt
!
!   data corresponds
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
        real(kind=kreal),   allocatable :: f_0(:)
!
        real(kind=kreal),   allocatable :: f_x(:)
        real(kind=kreal),   allocatable :: f_y(:)
        real(kind=kreal),   allocatable :: f_z(:)
        real(kind=kreal),   allocatable :: f_x2(:)
        real(kind=kreal),   allocatable :: f_y2(:)
        real(kind=kreal),   allocatable :: f_z2(:)
        real(kind=kreal),   allocatable :: f_xy(:)
        real(kind=kreal),   allocatable :: f_yz(:)
        real(kind=kreal),   allocatable :: f_zx(:)
      end type filter_mom_type
!
      type filter_mom_diffs_type
        real(kind=kreal),   allocatable :: df_0(:,:)
!
        real(kind=kreal),   allocatable :: df_x(:,:)
        real(kind=kreal),   allocatable :: df_y(:,:)
        real(kind=kreal),   allocatable :: df_z(:,:)
        real(kind=kreal),   allocatable :: df_x2(:,:)
        real(kind=kreal),   allocatable :: df_y2(:,:)
        real(kind=kreal),   allocatable :: df_z2(:,:)
        real(kind=kreal),   allocatable :: df_xy(:,:)
        real(kind=kreal),   allocatable :: df_yz(:,:)
        real(kind=kreal),   allocatable :: df_zx(:,:)
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
!
      subroutine copy_moments_each_point                                &
     &         (i_org, mom_org, i_tgt, mom_tgt)
!
      integer (kind = kint), intent(in) :: i_org, i_tgt
      type(filter_mom_type), intent(in) :: mom_org
      type(filter_mom_type), intent(inout) :: mom_tgt
!
      mom_tgt%f_0(i_tgt) =  mom_org%f_0(i_org)
      mom_tgt%f_x(i_tgt) =  mom_org%f_x(i_org)
      mom_tgt%f_y(i_tgt) =  mom_org%f_y(i_org)
      mom_tgt%f_z(i_tgt) =  mom_org%f_z(i_org)
      mom_tgt%f_x2(i_tgt) = mom_org%f_x2(i_org)
      mom_tgt%f_y2(i_tgt) = mom_org%f_y2(i_org)
      mom_tgt%f_z2(i_tgt) = mom_org%f_z2(i_org)
      mom_tgt%f_xy(i_tgt) = mom_org%f_xy(i_org)
      mom_tgt%f_yz(i_tgt) = mom_org%f_yz(i_org)
      mom_tgt%f_zx(i_tgt) = mom_org%f_zx(i_org)
!
      end subroutine copy_moments_each_point
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mom_diffs_each_point                              &
     &         (i_org, mom_org, i_tgt, mom_tgt)
!
      integer (kind = kint), intent(in) :: i_org, i_tgt
      type(filter_mom_diffs_type), intent(in) :: mom_org
      type(filter_mom_diffs_type), intent(inout) :: mom_tgt
!
      integer (kind=kint) :: nd
!
!
      do nd = 1, 3
        mom_tgt%df_x(i_tgt,nd) =  mom_org%df_x(i_org,nd)
        mom_tgt%df_y(i_tgt,nd) =  mom_org%df_y(i_org,nd)
        mom_tgt%df_z(i_tgt,nd) =  mom_org%df_z(i_org,nd)
        mom_tgt%df_x2(i_tgt,nd) = mom_org%df_x2(i_org,nd)
        mom_tgt%df_y2(i_tgt,nd) = mom_org%df_y2(i_org,nd)
        mom_tgt%df_z2(i_tgt,nd) = mom_org%df_z2(i_org,nd)
        mom_tgt%df_xy(i_tgt,nd) = mom_org%df_xy(i_org,nd)
        mom_tgt%df_yz(i_tgt,nd) = mom_org%df_yz(i_org,nd)
        mom_tgt%df_zx(i_tgt,nd) = mom_org%df_zx(i_org,nd)
      end do
!
      end subroutine copy_mom_diffs_each_point
!
!  ---------------------------------------------------------------------
!
      subroutine copy_moments_type(num, mom_org, mom_tgt)
!
      integer (kind = kint), intent(in) :: num
      type(filter_mom_type), intent(in) :: mom_org
      type(filter_mom_type), intent(inout) :: mom_tgt
!
      integer (kind=kint) :: i
!
!
!$omp parallel do
      do i = 1, num
        mom_tgt%f_0(i) =  mom_org%f_0(i)
        mom_tgt%f_x(i) =  mom_org%f_x(i)
        mom_tgt%f_y(i) =  mom_org%f_y(i)
        mom_tgt%f_z(i) =  mom_org%f_z(i)
        mom_tgt%f_x2(i) = mom_org%f_x2(i)
        mom_tgt%f_y2(i) = mom_org%f_y2(i)
        mom_tgt%f_z2(i) = mom_org%f_z2(i)
        mom_tgt%f_xy(i) = mom_org%f_xy(i)
        mom_tgt%f_yz(i) = mom_org%f_yz(i)
        mom_tgt%f_zx(i) = mom_org%f_zx(i)
      end do
!$omp end parallel do
!
      end subroutine copy_moments_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mom_diffs_type(num, mom_org, mom_tgt)
!
      integer (kind = kint), intent(in) :: num
      type(filter_mom_diffs_type), intent(in) :: mom_org
      type(filter_mom_diffs_type), intent(inout) :: mom_tgt
!
      integer (kind=kint) :: nd, i
!
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do
        do i = 1, num
          mom_tgt%df_x(i,nd) =  mom_org%df_x(i,nd)
          mom_tgt%df_y(i,nd) =  mom_org%df_y(i,nd)
          mom_tgt%df_z(i,nd) =  mom_org%df_z(i,nd)
          mom_tgt%df_x2(i,nd) = mom_org%df_x2(i,nd)
          mom_tgt%df_y2(i,nd) = mom_org%df_y2(i,nd)
          mom_tgt%df_z2(i,nd) = mom_org%df_z2(i,nd)
          mom_tgt%df_xy(i,nd) = mom_org%df_xy(i,nd)
          mom_tgt%df_yz(i,nd) = mom_org%df_yz(i,nd)
          mom_tgt%df_zx(i,nd) = mom_org%df_zx(i,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_mom_diffs_type
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
      subroutine alloc_filter_mom_ele_items(nele, n_filter, mom_ele)
!
      integer(kind = kint), intent(in) :: nele, n_filter
      type(ele_mom_diffs_type), intent(inout) :: mom_ele(n_filter)
      integer(kind = kint) :: i
!
!
      do i = 1, n_filter
        call alloc_moments_type(nele, mom_ele(i)%moms)
        call alloc_filter_mom_diffs_type(nele, mom_ele(i)%diff)
        call alloc_filter_mom_diffs_type(nele, mom_ele(i)%diff2)
      end do
!
      end subroutine alloc_filter_mom_ele_items
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_mom_ele_items(n_filter, mom_ele)
!
      integer(kind = kint), intent(in) :: n_filter
      type(ele_mom_diffs_type), intent(inout) :: mom_ele(n_filter)
      integer(kind = kint) :: i
!
!
      do i = 1, n_filter
        call dealloc_moments_type(mom_ele(i)%moms)
        call dealloc_filter_mom_diffs_type(mom_ele(i)%diff)
        call dealloc_filter_mom_diffs_type(mom_ele(i)%diff2)
      end do
!
      end subroutine dealloc_filter_mom_ele_items
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_moms_ele_type(nele, FEM_moms)
!
      integer(kind = kint), intent(in) :: nele
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      FEM_moms%nele_fmom = nele
!
      allocate( FEM_moms%mom_ele(FEM_moms%num_filter_moms) )
      call alloc_filter_mom_ele_items(FEM_moms%nele_fmom,               &
     &    FEM_moms%num_filter_moms, FEM_moms%mom_ele)
!
      end subroutine alloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_moms_ele_type(FEM_moms)
!
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      call dealloc_filter_mom_ele_items                                 &
     &   (FEM_moms%num_filter_moms, FEM_moms%mom_ele)
      deallocate( FEM_moms%mom_ele )
!
      end subroutine dealloc_filter_moms_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_ele                                   &
     &         (nele, n_filter, org_mom_ele, tgt_mom_ele)
!
      integer(kind = kint), intent(in) :: nele, n_filter
      type(ele_mom_diffs_type), intent(in) ::    org_mom_ele(n_filter)
      type(ele_mom_diffs_type), intent(inout) :: tgt_mom_ele(n_filter)
      integer(kind = kint) :: ifil
!
!
      do ifil = 1, n_filter
        call copy_moments_type                                          &
     &     (nele, org_mom_ele(ifil)%moms, tgt_mom_ele(ifil)%moms)
        call copy_mom_diffs_type                                        &
     &     (nele, org_mom_ele(ifil)%diff, tgt_mom_ele(ifil)%diff)
        call copy_mom_diffs_type                                        &
     &     (nele, org_mom_ele(ifil)%diff2, tgt_mom_ele(ifil)%diff2)
      end do
!
      end subroutine copy_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_moms_nod_type(FEM_moms_org, FEM_moms_tgt)
!
      type(gradient_filter_mom_type), intent(in) ::    FEM_moms_org
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms_tgt
      integer(kind = kint) :: ifil
!
!
      FEM_moms_tgt%nnod_fmom = FEM_moms_org%nnod_fmom
      do ifil = 1, FEM_moms_tgt%num_filter_moms
        call copy_moments_type(FEM_moms_tgt%nnod_fmom,                  &
     &      FEM_moms_org%mom_nod(ifil)%moms,                            &
     &      FEM_moms_tgt%mom_nod(ifil)%moms)
        call copy_mom_diffs_type(FEM_moms_tgt%nnod_fmom,                &
     &      FEM_moms_org%mom_nod(ifil)%diff,                            &
     &      FEM_moms_tgt%mom_nod(ifil)%diff)
      end do
!
      end subroutine copy_filter_moms_nod_type
!
!  ---------------------------------------------------------------------
!
      end module t_filter_moments
