!
!     module read_sphere_surface
!
!     Written by H. Matsui
!
!      subroutine read_surface_connect_linear
!      subroutine read_surface_connect_quad
!
      module read_sphere_surface
!
      use m_precision
!
      use m_shell_surface
!
      implicit none
!
      character(len=kchara) :: sphere_data_file_name
!
      character(len=kchara) :: tmp_character
      integer(kind = kint) :: itmp
      private :: tmp_character, itmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_surface_connect_linear
!
      integer(kind = kint) :: i
!
!
      open (21,file=sphere_data_file_name)
!
      read(21,*) tmp_character
      read(21,*) nnod_cube, nedge_cube
!
      read(21,*) tmp_character
      read(21,*) nnod_CMB, nedge_CMB, num_layer
!
      read(21,*) tmp_character
      read(21,*) nele_CMB
!
      read(21,*) tmp_character
      read(21,*) nlayer_ICB, nlayer_CMB
!
      num_cube = nnod_cube
      num_CMB = nnod_CMB
!
      call allocate_linear_shell
!
      read(21,*) tmp_character
      do i = 1, num_CMB
        read(21,*) itmp, xx_cmb(i,1:3)
      end do
!
      close(21)
!
      end subroutine read_surface_connect_linear
!
!  ---------------------------------------------------------------------
!
      subroutine read_surface_connect_quad
!
      integer(kind = kint) :: i
!
!
       open (21,file=sphere_data_file_name)
!
       read(21,*) tmp_character
       read(21,*) nnod_cube, nedge_cube
!
       read(21,*) tmp_character
       read(21,*) nnod_CMB, nedge_CMB, num_layer
!
       read(21,*) tmp_character
       read(21,*) nele_CMB
!
       read(21,*) tmp_character
       read(21,*) nlayer_ICB, nlayer_CMB
!
       num_cube = nnod_cube + nedge_cube
       num_CMB = nnod_CMB + nedge_CMB
!
      call allocate_linear_shell
      call allocate_quad_shell
!
      read(21,*) tmp_character
      do i = 1, num_CMB
        read(21,*) itmp, xx_cmb(i,1:3)
      end do
!
      close(21)
!
      end subroutine read_surface_connect_quad
!
!  ---------------------------------------------------------------------
!
      end module read_sphere_surface
