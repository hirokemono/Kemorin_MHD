!set_export_cube.f90
!     module set_export_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_export_data(nb_rng, ipe, jpe)
!!      subroutine set_export_data_quad(nb_rng, ipe, jpe, kpe)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_export_cube
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use count_export_inside_cube
      use set_export_inside_cube
      use count_export_peri
      use set_export_peri_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_export_data(nb_rng, ipe, jpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
!
      integer (kind = kint) :: inod
!
!
      neibpetot = 0
      inod   = 0
      call count_export_inside(nb_rng, neibpetot, inod)
      call count_export_peri_linear                                     &
     &   (nb_rng, ipe, jpe, neibpetot, inod)
      num_export = stack_export(neibpetot)
!
      inod = 0
      neibpetot = 0
      call set_export_inside(nb_rng, neibpetot, inod)
      call set_export_peri(nb_rng, ipe, jpe, neibpetot, inod)
!
      end subroutine set_export_data
!
! ----------------------------------------------------------------------
!
      subroutine set_export_data_quad(nb_rng, ipe, jpe, kpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: inod
!
! ***** set and write export nodes
!                                     .... count nodes 
      inod = 0
      neibpetot = 0
      call count_export_inside_quad(nb_rng, kpe, neibpetot, inod)
      call count_export_peri_quad                                       &
     &   (nb_rng, ipe, jpe, kpe, neibpetot, inod)

      num_export = stack_export(neibpetot)
!
      inod = 0
      neibpetot = 0
      call set_export_inside_quad(nb_rng, kpe, neibpetot, inod)
      call set_export_peri_quad                                         &
     &   (nb_rng, ipe, jpe, kpe, neibpetot, inod)
!
      end subroutine set_export_data_quad
!
! ----------------------------------------------------------------------
!
      end module set_export_cube
