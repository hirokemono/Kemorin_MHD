!m_ctl_param_partitioner.f90
!      module m_ctl_param_partitioner
!
!      Written by H. Matsui on Aug., 2007
!
      module m_ctl_param_partitioner
!
      use m_precision
      use t_ctl_param_partitioner
      use t_file_IO_parameter
!
      implicit none
!
!
      integer(kind = kint) :: num_domain
      character(len=kchara) :: org_mesh_header
!
      integer(kind = kint) :: iflag_viewer_output = 0
!
      integer(kind = kint) :: n_overlap =    1
      integer(kind = kint) :: i_sleeve_ele = 0
!
      integer(kind = kint) :: NTYP_div
!
!
      integer(kind = kint) :: iflag_sphere_data
      integer(kind = kint) :: iflag_new_partition
      integer(kind = kint) :: iflag_new_ghost_cell
!
!
!
      type(field_IO_params), save :: global_mesh_file
      type(field_IO_params), save :: distribute_mesh_file
!
      type(field_IO_params), save :: finer_mesh_file
!
      type(ctl_param_partitioner), save :: part_p1
!
      end module m_ctl_param_partitioner
