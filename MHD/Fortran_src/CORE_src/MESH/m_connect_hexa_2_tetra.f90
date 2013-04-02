!m_connect_hexa_2_tetra.f90
!      module m_connect_hexa_2_tetra
!
!> @brief  Index table to devide one hexahedron to tetrahidra
!
!>   linear element ->      5 tetrahidra
!>   quad element ->       21 tetrahidra
!>   laglandge element ->  40 tetrahidra
!
!      Written by H. Matsui
!
!      subroutine set_1_hexa_2_5_tetra
!      subroutine set_1_hexa_2_21_tetra
!      subroutine set_1_hexa_2_40_tetra
!
!      subroutine deallocate_hex_2_tetra
!
      module m_connect_hexa_2_tetra
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: num_tetra
!<   number of tetrahidra from one hexahedron element
      integer(kind = kint), allocatable :: ie_tetra(:,:)
!<   local index to construct tetrahidra from one hexahedron element
!<     ie_tetra(i,j): i...tetrahidra ID, j...devided ID
!
      integer(kind = kint), parameter :: ie_hex_2_5tetra(20)            &
     &     = (/  1,  2,  3,  6,     3,  4,  1,  8,     6,  5,  8,  1,   &
     &           7,  6,  8,  3,     1,  3,  8,  6 /)
!<     index table for linear element
!
!
      integer(kind = kint), parameter :: ie_hex_2_21tetra(84)           &
     &     = (/  9, 10, 11, 14,    11, 12,  9, 16,    14, 13, 16,  9,   &
     &          15, 14, 16, 11,     9, 11, 16, 14,     9, 13, 16, 17,   &
     &          16, 12,  9, 17,     9, 10, 14, 18,    14, 13,  9, 18,   &
     &          10, 11, 14, 19,    11, 15, 14, 19,    11, 12, 16, 20,   &
     &          16, 15, 11, 20,     1,  9, 12, 17,     2, 10,  9, 18,   &
     &           3, 11, 10, 19,     4, 12, 11, 20,     5, 16, 13, 17,   &
     &           6, 13, 14, 18,     7, 14, 15, 19,     8, 15, 16, 20 /)
!<     index table for quad element
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_1_hexa_2_5_tetra
!
      integer (kind=kint) :: i
!
      num_tetra = 5
      allocate( ie_tetra(4,num_tetra) )
!
      do i = 1, num_tetra
        ie_tetra(1,i) = ie_hex_2_5tetra(i*4-3)
        ie_tetra(2,i) = ie_hex_2_5tetra(i*4-2)
        ie_tetra(3,i) = ie_hex_2_5tetra(i*4-1)
        ie_tetra(4,i) = ie_hex_2_5tetra(i*4  )
      end do
!
      end subroutine set_1_hexa_2_5_tetra
!
! ----------------------------------------------------------------------
!
      subroutine set_1_hexa_2_21_tetra
!
      integer (kind=kint) :: i
!
      num_tetra = 21
      allocate( ie_tetra(4,num_tetra) )
!
      do i = 1, num_tetra
        ie_tetra(1,i) = ie_hex_2_21tetra(i*4-3)
        ie_tetra(2,i) = ie_hex_2_21tetra(i*4-2)
        ie_tetra(3,i) = ie_hex_2_21tetra(i*4-1)
        ie_tetra(4,i) = ie_hex_2_21tetra(i*4  )
      end do
!
      end subroutine set_1_hexa_2_21_tetra
!
! ----------------------------------------------------------------------
!
      subroutine set_1_hexa_2_40_tetra
!
      use m_27quad_2_8x8linear
!
      integer (kind=kint) :: i, j, k, i1, i2, i3, i4
!
      num_tetra = 40
      allocate( ie_tetra(4,num_tetra) )
!
      do j = 1, 8
        do i = 1, 5
          k = (j-1)*5 + i
          i1 = 8*(j-i) + ie_hex_2_5tetra(i*4-3)
          i2 = 8*(j-i) + ie_hex_2_5tetra(i*4-2)
          i3 = 8*(j-i) + ie_hex_2_5tetra(i*4-1)
          i4 = 8*(j-i) + ie_hex_2_5tetra(i*4  )
          ie_tetra(1,i) = id_quad27_8linear8(i1)
          ie_tetra(2,i) = id_quad27_8linear8(i2)
          ie_tetra(3,i) = id_quad27_8linear8(i3)
          ie_tetra(4,i) = id_quad27_8linear8(i4)
        end do
      end do
!
      end subroutine set_1_hexa_2_40_tetra
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_hex_2_tetra
!
      deallocate(ie_tetra)
!
      end subroutine deallocate_hex_2_tetra
!
! ----------------------------------------------------------------------
!
      end module m_connect_hexa_2_tetra
