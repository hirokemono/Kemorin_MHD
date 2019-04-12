!set_import_cube.f90
!     module set_import_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_import_data(ipe, jpe)
!      subroutine set_import_data_quad(ipe, jpe, kpe)
!
      module set_import_cube
!
      use m_precision
!
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use count_import_inside_cube
      use set_import_inside_cube
      use count_import_peri
      use set_import_peri_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data(ipe, jpe)
!
      integer(kind = kint), intent(in) :: ipe, jpe
!
      integer(kind = kint) :: inod
!
!
            neibpetot = 0
            inod   = 0

            call count_import_inside(inod)

            call count_import_peri_linear(ipe, jpe, inod)
!
            num_import = stack_import(neibpetot)
!
!                                     .... write nodes 
            neibpetot = 0
            inod = 0

            call set_import_inside(inod)

            call set_import_peri(ipe, jpe, inod)
!
          end subroutine set_import_data
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data_quad(ipe, jpe, kpe)
!
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: inod
!
!
!                                     .... count nodes 
            inod = 0
            neibpetot = 0
!
            call count_import_inside_quad(kpe, inod)
!
            call count_import_peri_quad(ipe, jpe, kpe, inod)
!
            num_import = stack_import(neibpetot)
            write(*,*) ipe, jpe, kpe, 'num_import', num_import
!
!
!                                     .... write nodes 
            inod = 0
            neibpetot = 0
!
!
            call set_import_inside_quad(kpe, inod)
!
            call set_import_peri_quad(ipe, jpe, kpe, inod)
!
            write(*,*) ipe, jpe, kpe, 'import res.', inod
!
      end subroutine set_import_data_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_cube
