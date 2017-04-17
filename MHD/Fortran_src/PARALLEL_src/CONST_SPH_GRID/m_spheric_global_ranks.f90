!>@file   m_spheric_global_ranks.f90
!!@brief  module m_spheric_global_ranks
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global subdomain informatikn for spherical shell
!!
!!@verbatim
!!      subroutine allocate_sph_ranks
!!      subroutine allocate_sph_1d_domain_id(sph_rtp, sph_rj)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine deallocate_sph_ranks
!!      subroutine deallocate_sph_1d_domain_id
!!
!!      subroutine check_sph_domains(nprocs_check, ierr, e_message)
!!      subroutine check_sph_ranks(my_rank)
!!      subroutine check_sph_1d_domain_id
!!@endverbatim
!
      module m_spheric_global_ranks
!
      use m_precision
      use t_spheric_global_ranks
!
      implicit none
!
!
      type(spheric_global_rank), save :: s3d_ranks
!
      end module m_spheric_global_ranks
