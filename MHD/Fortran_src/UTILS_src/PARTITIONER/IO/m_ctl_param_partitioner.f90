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
!
      integer(kind = kint) :: NTYP_div
!
!
      type(ctl_param_partitioner), save :: part_p1
!
      end module m_ctl_param_partitioner
