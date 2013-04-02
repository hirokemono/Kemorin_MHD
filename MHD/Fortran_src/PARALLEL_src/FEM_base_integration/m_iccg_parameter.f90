!
!     module   m_iccg_parameter
!.......................................................................
!
!     Written by H. Matsui and H. Okuda in 2000
!
      module   m_iccg_parameter
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal)   :: eps
      integer(kind=kint) :: itr
      real(kind=kreal)   :: sigma
      real(kind=kreal)   :: sigma_diag
! 
      character (len=kchara)   :: precond
      character (len=kchara)   :: method
! 
!
      real(kind=kreal), dimension(3)   :: init_4_solver11_real
      integer(kind=kint), dimension(2)   :: init_4_solver11_int
! 
!
      real(kind=kreal), dimension(3)   :: init_4_solver33_real
      integer(kind=kint), dimension(2)   :: init_4_solver33_int
! 
!      solver and precionditioning for Poisson matrix
!
      character (len=kchara) :: precond_4_solver
      character (len=kchara) :: method_4_solver
! 
!      solver and precionditioning for 3x3 matrix
!
      character (len=kchara) :: precond_4_crank
      character (len=kchara) :: method_4_velo
! 
      real(kind=kreal)     :: eps_crank
! 
      real(kind=kreal)   :: eps_4_velo_crank
      real(kind=kreal)   :: eps_4_magne_crank
      real(kind=kreal)   :: eps_4_temp_crank
      real(kind=kreal)   :: eps_4_d_scalar_crank
!
!
      integer(kind=kint) :: itr_res
!
!
!
      integer (kind=kint) :: iflag_ordering
      integer (kind=kint) :: min_color
      integer (kind=kint) :: mc_color
!
      end module m_iccg_parameter
