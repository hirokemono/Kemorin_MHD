!
!      module m_filter_moments
!
!     Written by H. Matsui
!
!      subroutine allocate_filter_moms_nod
!      subroutine allocate_filter_moms_ele
!
!      subroutine deallocate_filter_moms_nod
!      subroutine deallocate_filter_moms_ele
!
!   data correspondence
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
      module m_filter_moments
!
      use m_precision
      use t_filter_moments
!
      implicit none
!
      type(gradient_filter_mom_type), save :: mom1
!   mom1%num_filter_moms
!
      real(kind=kreal),   allocatable :: filter_x_nod(:,:)
      real(kind=kreal),   allocatable :: filter_y_nod(:,:)
      real(kind=kreal),   allocatable :: filter_z_nod(:,:)
      real(kind=kreal),   allocatable :: filter_x2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_y2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_z2_nod(:,:)
      real(kind=kreal),   allocatable :: filter_xy_nod(:,:)
      real(kind=kreal),   allocatable :: filter_yz_nod(:,:)
      real(kind=kreal),   allocatable :: filter_zx_nod(:,:)
!
      real(kind=kreal),   allocatable :: filter_x_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_nod_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_nod_dx(:,:,:)
!
!
      real(kind=kreal),   allocatable :: filter_x_ele(:,:)
      real(kind=kreal),   allocatable :: filter_y_ele(:,:)
      real(kind=kreal),   allocatable :: filter_z_ele(:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele(:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele(:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele(:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele(:,:)
!
      real(kind=kreal),   allocatable :: filter_x_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele_dx(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele_dx(:,:,:)
!
      real(kind=kreal),   allocatable :: filter_x_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_y_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_z_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_x2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_y2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_z2_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_xy_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_yz_ele_dx2(:,:,:)
      real(kind=kreal),   allocatable :: filter_zx_ele_dx2(:,:,:)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_nod(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
!
      mom1%nnod_fmom = nnod
      allocate( filter_x_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_y_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_z_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_x2_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_y2_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_z2_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_xy_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_yz_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
      allocate( filter_zx_nod(mom1%nnod_fmom,mom1%num_filter_moms) )
!
      allocate( filter_x_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_x2_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y2_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z2_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_xy_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_yz_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
      allocate( filter_zx_nod_dx(mom1%nnod_fmom,3,mom1%num_filter_moms) )
!
      filter_x_nod = 0.0d0
      filter_y_nod = 0.0d0
      filter_z_nod = 0.0d0
      filter_x2_nod = 0.0d0
      filter_y2_nod = 0.0d0
      filter_z2_nod = 0.0d0
      filter_xy_nod = 0.0d0
      filter_yz_nod = 0.0d0
      filter_zx_nod = 0.0d0
!
      filter_x_nod_dx = 0.0d0
      filter_y_nod_dx = 0.0d0
      filter_z_nod_dx = 0.0d0
      filter_x2_nod_dx = 0.0d0
      filter_y2_nod_dx = 0.0d0
      filter_z2_nod_dx = 0.0d0
      filter_xy_nod_dx = 0.0d0
      filter_yz_nod_dx = 0.0d0
      filter_zx_nod_dx = 0.0d0
!
      end subroutine allocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_ele(nele)
!
      integer(kind = kint), intent(in) :: nele
!
!
      mom1%nele_fmom = nele
      allocate( filter_x_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_y_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_z_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_x2_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_y2_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_z2_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_xy_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_yz_ele(mom1%nele_fmom,mom1%num_filter_moms) )
      allocate( filter_zx_ele(mom1%nele_fmom,mom1%num_filter_moms) )
!
      allocate( filter_x_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_x2_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y2_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z2_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_xy_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_yz_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_zx_ele_dx(mom1%nele_fmom,3,mom1%num_filter_moms) )
!
      allocate( filter_x_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_x2_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_y2_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_z2_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_xy_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_yz_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
      allocate( filter_zx_ele_dx2(mom1%nele_fmom,3,mom1%num_filter_moms) )
!
      filter_x_ele =  0.0d0
      filter_y_ele =  0.0d0
      filter_z_ele =  0.0d0
      filter_x2_ele = 0.0d0
      filter_y2_ele = 0.0d0
      filter_z2_ele = 0.0d0
      filter_xy_ele = 0.0d0
      filter_yz_ele = 0.0d0
      filter_zx_ele = 0.0d0
!
      filter_x_ele_dx =  0.0d0
      filter_y_ele_dx =  0.0d0
      filter_z_ele_dx =  0.0d0
      filter_x2_ele_dx = 0.0d0
      filter_y2_ele_dx = 0.0d0
      filter_z2_ele_dx = 0.0d0
      filter_xy_ele_dx = 0.0d0
      filter_yz_ele_dx = 0.0d0
      filter_zx_ele_dx = 0.0d0
!
      filter_x_ele_dx2 =  0.0d0
      filter_y_ele_dx2 =  0.0d0
      filter_z_ele_dx2 =  0.0d0
      filter_x2_ele_dx2 = 0.0d0
      filter_y2_ele_dx2 = 0.0d0
      filter_z2_ele_dx2 = 0.0d0
      filter_xy_ele_dx2 = 0.0d0
      filter_yz_ele_dx2 = 0.0d0
      filter_zx_ele_dx2 = 0.0d0
!
      end subroutine allocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_nod
!
      deallocate( filter_x_nod )
      deallocate( filter_y_nod )
      deallocate( filter_z_nod )
      deallocate( filter_x2_nod )
      deallocate( filter_y2_nod )
      deallocate( filter_z2_nod )
      deallocate( filter_xy_nod )
      deallocate( filter_yz_nod )
      deallocate( filter_zx_nod )
!
      deallocate( filter_x_nod_dx )
      deallocate( filter_y_nod_dx )
      deallocate( filter_z_nod_dx )
      deallocate( filter_x2_nod_dx )
      deallocate( filter_y2_nod_dx )
      deallocate( filter_z2_nod_dx )
      deallocate( filter_xy_nod_dx )
      deallocate( filter_yz_nod_dx )
      deallocate( filter_zx_nod_dx )
!
      end subroutine deallocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_ele
!
      deallocate( filter_x_ele )
      deallocate( filter_y_ele )
      deallocate( filter_z_ele )
      deallocate( filter_x2_ele )
      deallocate( filter_y2_ele )
      deallocate( filter_z2_ele )
      deallocate( filter_xy_ele )
      deallocate( filter_yz_ele )
      deallocate( filter_zx_ele )
!
      deallocate( filter_x_ele_dx )
      deallocate( filter_y_ele_dx )
      deallocate( filter_z_ele_dx )
      deallocate( filter_x2_ele_dx )
      deallocate( filter_y2_ele_dx )
      deallocate( filter_z2_ele_dx )
      deallocate( filter_xy_ele_dx )
      deallocate( filter_yz_ele_dx )
      deallocate( filter_zx_ele_dx )
!
      deallocate( filter_x_ele_dx2 )
      deallocate( filter_y_ele_dx2 )
      deallocate( filter_z_ele_dx2 )
      deallocate( filter_x2_ele_dx2 )
      deallocate( filter_y2_ele_dx2 )
      deallocate( filter_z2_ele_dx2 )
      deallocate( filter_xy_ele_dx2 )
      deallocate( filter_yz_ele_dx2 )
      deallocate( filter_zx_ele_dx2 )
!
      end subroutine deallocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      end module m_filter_moments
