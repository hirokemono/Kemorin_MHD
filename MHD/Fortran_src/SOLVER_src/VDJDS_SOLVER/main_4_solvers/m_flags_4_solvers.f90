!>@file   m_flags_4_solvers.F90
!!@brief  module m_flags_4_solvers
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  character flags for solvers
!!
!!@verbatim
!!      integer(kind = kint) function solver_iflag(method)
!!@endverbatim
!
      module m_flags_4_solvers
!
      use m_precision
!
      implicit none
!
!>      Character flag for MGCG
      character(len=kchara), parameter :: cflag_mgcg =      'MGCG'
!>      Character flag for direct solver
      character(len=kchara), parameter :: cflag_lu_decomp = 'LU'
!
!>      Character flag for CG
      character(len=kchara), parameter :: cflag_cg = 'CG'
!>      Character flag for BiCGSTAB
      character(len=kchara), parameter :: cflag_bicgstab = 'BiCGSTAB'
!>      Character flag for CG
      character(len=kchara), parameter :: cflag_gpbicg = 'GPBiCG'
!
!>      Character flag for CG
      character(len=kchara), parameter :: cflag_cg_NN = 'CG_NN'
!>      Character flag for BiCGSTAB
      character(len=kchara), parameter                                  &
     &                             :: cflag_bicgstab_NN = 'BiCGSTAB_NN'
!>      Character flag for CG
      character(len=kchara), parameter :: cflag_gpbicg_NN = 'GPBiCG_NN'
!
!>      Character flag for Gauss-zeidel
      character(len=kchara), parameter :: cflag_gausszeidel = 'Gauss'
!>      Character flag for Jacobi
      character(len=kchara), parameter :: cflag_jacobi = 'Jacobi'
!
!
!>      Integer flag for MGCG
      integer(kind = kint), parameter :: iflag_mgcg =      99
!>      Integer flag for direct solver
      integer(kind = kint), parameter :: iflag_lu_decomp = 0
!
!>      Integer flag for CG
      integer(kind = kint), parameter :: iflag_cg = 1
!>      Integer flag for BiCGSTAB
      integer(kind = kint), parameter :: iflag_bicgstab = 2
!>      Integer flag for CG
      integer(kind = kint), parameter :: iflag_gpbicg = 3
!
!>      Integer flag for CG
      integer(kind = kint), parameter :: iflag_cg_NN = 101
!>      Integer flag for BiCGSTAB
      integer(kind = kint), parameter :: iflag_bicgstab_NN = 102
!>      Integer flag for CG
      integer(kind = kint), parameter :: iflag_gpbicg_NN = 103
!
!>      Integer flag for Gauss-zeidel
      integer(kind = kint), parameter :: iflag_gausszeidel = 4
!>      Integer flag for Jacobi
      integer(kind = kint), parameter :: iflag_jacobi = 5
!
!>      Integer flag for undefined
      integer(kind = kint), parameter :: iflag_undefined = -1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function solver_iflag(method)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: method
!
!
      if(cmp_no_case(method, cflag_mgcg)) then
        solver_iflag = iflag_mgcg
!
      else if(cmp_no_case(method, cflag_lu_decomp)) then
        solver_iflag = iflag_lu_decomp
!
      else if(cmp_no_case(method, cflag_cg)) then
        solver_iflag = iflag_cg
      else if(cmp_no_case(method, cflag_bicgstab)) then
        solver_iflag = iflag_bicgstab
      else if(cmp_no_case(method, cflag_gpbicg)) then
        solver_iflag = iflag_gpbicg
!
      else if(cmp_no_case(method, cflag_cg_NN)) then
        solver_iflag = iflag_cg_NN
      else if(cmp_no_case(method, cflag_bicgstab_NN)) then
        solver_iflag = iflag_bicgstab_NN
      else if(cmp_no_case(method, cflag_gpbicg_NN)) then
        solver_iflag = iflag_gpbicg_NN
!
      else if(cmp_no_case(method, cflag_gausszeidel)) then
        solver_iflag = iflag_gausszeidel
      else if(cmp_no_case(method, cflag_jacobi)) then
        solver_iflag = iflag_jacobi
      else
        solver_iflag = iflag_undefined
      end if
!
      end function solver_iflag
!
!  ---------------------------------------------------------------------
!
      end module m_flags_4_solvers
