!
!      module cubed_sph_file_names
!
!      subroutine set_linear_mesh_file_names
!      subroutine set_quad_mesh_file_names
!      subroutine set_coarse_mesh_names(ic_level)
!
      module cubed_sph_file_names
!
      use m_precision
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint), parameter :: id_l_mesh = 7
      integer(kind = kint), parameter :: id_q_mesh = 8
      integer(kind = kint), parameter :: id_l_connect = 17
      integer(kind = kint), parameter :: id_q_connect = 18
      integer(kind = kint), parameter :: id_l_group = 27
      integer(kind = kint), parameter :: id_q_group = 28
!
      integer(kind = kint), parameter :: id_transfer = 49
!
      integer(kind = kint), parameter :: izero = 0
!
      character(len = kchara), parameter :: fh_l_mesh =    'mesh'
      character(len = kchara), parameter :: fh_q_mesh =    'mesh_20'
      character(len = kchara), parameter :: fh_l_connect = 'connect'
      character(len = kchara), parameter :: fh_q_connect = 'connect_20'
      character(len = kchara), parameter :: fh_l_group =   'group'
      character(len = kchara), parameter :: fh_q_group =   'group_20'
!
      character(len = kchara), parameter :: fh_transfer =  'transfer'
!
      character(len = kchara) :: fn_l_mesh
      character(len = kchara) :: fn_q_mesh
      character(len = kchara) :: fn_l_connect
      character(len = kchara) :: fn_q_connect
      character(len = kchara) :: fn_l_group
      character(len = kchara) :: fn_q_group
!
      character(len = kchara) :: fn_transfer
!
      private :: fh_l_mesh, fh_l_connect, fh_l_group
      private :: fh_q_mesh, fh_q_connect, fh_q_group, fh_transfer
      private :: fn_l_mesh, fn_l_connect, fn_l_group
      private :: fn_q_mesh, fn_q_connect, fn_q_group, fn_transfer
      private :: izero
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_linear_mesh_file_names
!
      call add_int_suffix(izero, fh_l_mesh,    fn_l_mesh)
      call add_int_suffix(izero, fh_l_connect, fn_l_connect)
      call add_int_suffix(izero, fh_l_group,   fn_l_group)
!
      open (id_l_mesh,file=fn_l_mesh)
      open (id_l_connect,file=fn_l_connect)
      open (id_l_group,file=fn_l_group)
!
      end subroutine set_linear_mesh_file_names
!
!   --------------------------------------------------------------------
!
      subroutine set_quad_mesh_file_names
!
      call add_int_suffix(izero, fh_q_mesh,    fn_q_mesh)
      call add_int_suffix(izero, fh_q_connect, fn_q_connect)
      call add_int_suffix(izero, fh_q_group,   fn_q_group)
!
      open (id_q_mesh,file=fn_q_mesh)
      open (id_q_connect,file=fn_q_connect)
      open (id_q_group,file=fn_q_group)
!
      end subroutine set_quad_mesh_file_names
!
!   --------------------------------------------------------------------
!
      subroutine set_coarse_mesh_names(ic_level)
!
      integer(kind = kint), intent(in) :: ic_level
      character(len = kchara) :: fname_tmp
!
!
      call add_int_suffix(ic_level, fh_l_mesh,    fname_tmp)
      call add_int_suffix(izero,    fname_tmp,    fn_l_mesh)
!
      call add_int_suffix(ic_level, fh_l_connect, fname_tmp)
      call add_int_suffix(izero,    fname_tmp,    fn_l_connect)
!
      call add_int_suffix(ic_level, fh_l_group,   fname_tmp)
      call add_int_suffix(izero,    fname_tmp,    fn_l_group)
!
      call add_int_suffix(ic_level, fh_transfer,   fname_tmp)
      call add_int_suffix(izero,    fname_tmp,    fn_transfer)
!
      open (id_l_mesh,file=fn_l_mesh)
      open (id_l_connect,file=fn_l_connect)
      open (id_l_group,file=fn_l_group)
      open (id_transfer,file=fn_transfer)
!
      end subroutine set_coarse_mesh_names
!
!   --------------------------------------------------------------------
!
      end module cubed_sph_file_names
