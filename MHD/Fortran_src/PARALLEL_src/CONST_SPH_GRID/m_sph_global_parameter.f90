!m_sph_global_parameter.f90
!      module m_sph_global_parameter
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine allocate_sph_gl_parameter
!!      subroutine allocate_sph_gl_bc_param
!!
!!      subroutine deallocate_sph_gl_parameter
!!      subroutine deallocate_sph_gl_bc_param
!
!
      module m_sph_global_parameter
!
      use m_precision
      use t_sph_local_parameter
!
      implicit none
!
      type(sph_local_parameters), save :: sph_lcp
!
      end module m_sph_global_parameter
